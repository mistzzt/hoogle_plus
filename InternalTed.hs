{-# LANGUAGE NamedFieldPuns, GeneralizedNewtypeDeriving #-}
module InternalTed where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData(..))

import Control.Monad.State ( evalState, State, MonadState(state) )
import Data.Bifunctor ( Bifunctor(bimap, first) )
import Data.List as L ( null )
import           Data.Map            (Map)
import qualified Data.Map            as M
import Data.Maybe ( fromJust, fromMaybe )
import           Data.Vector         (Vector)
import qualified Data.Vector         as V

cost :: Eq l => Op l -> Int
cost (Ins _) = 1
cost (Del _) = 1
cost (Rep f t) | f == t = 0
               | otherwise = 2

customTreeDist :: Eq l => Tree l -> Tree l -> Int
customTreeDist x1 x2 = treeDist x1 x2 cost

-- * Trees and Forests

-- | A 'Tree' is a node labelled with an element of 'sigma'.
data Tree sigma = Node
  { treeLabel    :: sigma
  , treeBranches :: Forest sigma
  }
  deriving (Ord, Eq)

-- | A 'Forest' is an ordered collection of 'Tree's.
newtype Forest sigma = Forest
  { forestTrees :: Vector (Tree sigma) }
  deriving (Ord, Eq)

newtype Idx = Idx { fromIdx :: Int }
  deriving (Eq, Ord, Num, Enum)

instance Show Idx where
  show = show . fromIdx

instance Read Idx where
  readsPrec i = map (first Idx) . filter (\(a,r) -> a >= 0) . readsPrec i

data FNode l = N
  { label         :: l
  , index         :: Idx
  , leftMostChild :: Idx
  , parent        :: Idx
  }
  deriving (Show)

-- | An array stored ascending by index.
--
-- The root of the tree is the last node.
newtype Flattened l = F (Vector (FNode l))
  deriving (Show)

countNodes :: Flattened l -> Int
countNodes (F v) = V.length v

-- | Find the root node
root :: Flattened l -> FNode l
root (F v) = V.last v

-- | Find the span of the sub-tree rooted at a node.
tree :: Flattened l -> Idx -> (Idx, Idx)
tree t id = (leftMostChild $ t `node` id, id)

-- | We'll flatten the tree in a left-right post-order traversal.
flattenTree :: Tree l -> Flattened l
flattenTree tree =
  let flat = flip evalState 0 . fmap fst . go $ tree
      root = V.last flat
      root' = root{parent = index root}
  in F $ V.init flat `V.snoc` root'
  where
    go :: Tree l -> State Idx (Vector (FNode l), Idx)
    go (Node l (Forest cs)) = do
      cs' <- V.mapM go cs
      next <- state (\next -> (next, next + 1))
      let left = maybe next snd $ cs' V.!? 0
      return (V.concatMap (fixup next) cs' `V.snoc` N l next left (-1), left)
    -- I should probably figure out how to tie this knot correctly.
    fixup :: Idx -> (Vector (FNode l), Idx) -> Vector (FNode l)
    fixup p = V.map (\n -> if parent n < 0 then n{parent = p} else n) . fst

-- | We access the elements of a flattened tree safely.
--
-- If we ever get a Nothing: 1) our program is wrong, 2) the answer
-- will be wrong, 3) screw you guys, I'm going home.
node :: Flattened l -> Idx -> FNode l
node (F v) (Idx n) =
  fromMaybe
    (error $ "Cannot get node " <> show n <> " from tree with " <> show (V.length v) <> " nodes")
    (v V.!? n)

-- | The key to this algorithm (and the newer better algorithms in the
-- same class) is in identifying the subset of sub-trees in that we
-- must process. This allows us to prune the search space considerably.
--
-- keyRoots(T) := { k | there is k' > k such that l(k) = l(k') }
--
-- i.e. k is either the root of T or it has a left-sibling (i.e. it's
-- own left-most child is different from it's parent's).
keyRoots :: Flattened l -> [Idx]
keyRoots tree@(F v) =
  map index . filter isKeyroot . V.toList $ v
  where
    isKeyroot :: FNode l -> Bool
    isKeyroot n = isRoot n || isKey n
    isRoot :: FNode l -> Bool
    isRoot node = index node == (index . root $ tree)
    isKey :: FNode l -> Bool
    isKey k =
      let lk = leftMostChild k
          lpk = leftMostChild (tree `node` parent k)
      in lk /= lpk

-- * Tree Distance

-- $ The algorithm proceeds by building a series of tableaux of
-- solutions to two types of sub-problems:
--
-- 1. a tableaux of sub-tree vs sub-tree distances; and
--
-- 2. a series of tableaux of sub-forest vs sub-forest distances.
--

-- | The edit operations.
--
-- These could be enhanced with a path to produce not just an optimal
-- edit distance but also a diff.
data Op l = Ins l
          | Del l
          | Rep l l

treeDist :: Eq a => Tree a -> Tree a -> (Op a -> Int) -> Int
treeDist in1 in2 cost =
  let t1 = flattenTree in1
      t2 = flattenTree in2
      kr1 = keyRoots t1
      kr2 = keyRoots t2

      -- | We'll describe the sub-problems to be evaluated from each
      -- pair of key roots. We can calculate these descriptions easily
      -- but we'll need to normalise them to have a single
      -- representation of the "empty" span.
      dep (l@(li, i), r@(lj, j), c) =
        let l' = if i < li then (li, -1) else l
            r' = if j < lj then (lj, -1) else r
        in (l', r', c)

      -- Solve a forest vs forest sub-problem.
      trees prev (i, j) =
        let li = leftMostChild $ t1 `node` i
            lj = leftMostChild $ t2 `node` j

            zeros   = [ ((li, -1), (lj, -1), []) ]
            deletes = [ ((li, m), (lj, -1), [
                            dep ((li, m-1), (lj, -1), cost (Del v1))
                        ])
                      | m <- [ li .. i ]
                      , let v1 = label (t1 `node` m)
                      ]
            inserts = [ ((li, -1), (lj, n), [
                            dep ((li, -1), (lj, n-1), cost (Ins v2))
                        ])
                      | n <- [ lj .. j ]
                      , let v2 = label (t2 `node` n)
                      ]
            changes = [ ((li, m), (lj, n),
                          if leftIsAWholeTree && rightIsAWholeTree
                          then [
                            dep ((li, m-1), (lj, n  ), cost (Del v1)),
                            dep ((li, m  ), (lj, n-1), cost (Ins v2)),
                            dep ((li, m-1), (lj, n-1), cost (Rep v1 v2))
                            ]
                          else [
                            dep ((li, m-1  ), (lj, n    ), cost (Del v1)),
                            dep ((li, m    ), (lj, n-1  ), cost (Ins v2)),
                            dep ((li, li1-1), (lj, lj1-1), treedist prev m n)
                            ]
                        )
                      | m <- [ li .. i ]
                      , n <- [ lj .. j ]
                      , let li1 = leftMostChild (t1 `node` m)
                            lj1 = leftMostChild (t2 `node` n)
                            leftIsAWholeTree = li1 == li
                            rightIsAWholeTree = lj1 == lj
                            v1 = label (t1 `node` m)
                            v2 = label (t2 `node` n)
                      ]
        in foldl (forests i j) prev $ zeros <> deletes <> inserts <> changes

      treedist prev i1 j1 =
        let key = (tree t1 i1, tree t2 j1)
        in fromMaybe (error $ "Cannot find previous subtree: " <> show key)
           (M.lookup key prev)
      forests i j prev subproblem@((li, i1), (lj, j1), cs)
        | i1 == -1 && j1 == -1 = M.insert ((li, -1), (lj, -1)) 0 prev
        | L.null cs = error $ "Expected to compute minimum but not children given: " <> show subproblem
        | otherwise =
            let f (from, to, c') =
                  case M.lookup (from, to) prev of
                    Just c -> c + c'
                    Nothing -> error $ "Missing subproblem: " <> show (from, to)
            in M.insert ((li, i1), (lj, j1)) (minimum $ map f cs) prev

      -- The forest vs forest sub-problems.
      ftabs = [ (r1, r2) | r1 <- kr1, r2 <- kr2 ]
  in 
    let forestDist   = foldl trees mempty ftabs
        (idx1, idx2) = (countNodes t1 - 1, countNodes t2 - 1)
        indices      = bimap (bimap Idx Idx) (bimap Idx Idx) ((0, idx1), (0, idx2))
    in fromJust (M.lookup indices forestDist)