{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable, LambdaCase, NamedFieldPuns #-}
module DraftSort where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (liftM2)
import Control.Monad.Logic (liftM)
import Data.Char (ord)
import Data.Containers.ListUtils (nubOrd)
import Data.Data (Data(..))
import Data.List (isInfixOf, elemIndex, nub, drop, reverse, intersect, intercalate)
import Text.Printf (printf)
import qualified Test.ChasingBottoms as CB
import qualified Test.QuickCheck as QC

import Test.SmallCheck
import Test.SmallCheck.Series
import Data.List (sortOn, sortBy, (\\), maximumBy, minimumBy)
import qualified Data.Map as Map
import Data.Bifunctor
import Debug.Trace
import Control.Lens
import Data.Ord

import TED.Data.Tree.Diff (n, treeDist, simpleTreeDist, Op(..))
import TED.Data.Tree (Tree(..))

data DataAnalysis =
  Instance  { typeName          :: String
            , constructorName   :: String
            , parameters        :: [DataAnalysis]
            , height            :: Int
            } deriving (Show, Eq)

convertDataAnalysis :: DataAnalysis -> Tree String
convertDataAnalysis Instance {constructorName, parameters} = n constructorName (map convertDataAnalysis parameters)

analyzeWithHeight :: (Analyze a) => a -> ([DataAnalysis], Int)
analyzeWithHeight a =
    let instances = [analyze a] in
        (instances, 1 + maximum (map height instances))

analyzeWithHeight2 :: (Analyze a, Analyze b) => a -> b -> ([DataAnalysis], Int)
analyzeWithHeight2 a b =
    let instances = [analyze a, analyze b] in
        (instances, 1 + maximum (map height instances))

class    Analyze a    where analyze :: a -> DataAnalysis
instance Analyze Int  where analyze x = Instance "Int"  (show $ x `compare` 0)  [] 0
instance Analyze Bool where analyze x = Instance "Bool" (show x)                [] 0
instance Analyze Char where analyze x = Instance "Char" "_"                     [] 0

instance Analyze a => Analyze [a] where
  analyze []        = Instance "List" "Nil"  [] 0
  analyze (x:xs)    = Instance "List" "Cons" p  h
    where (p, h)    = analyzeWithHeight2 x xs

instance Analyze a => Analyze (Maybe a) where
  analyze Nothing   = Instance "Maybe" "Nothing" [] 0
  analyze (Just x)  = Instance "Maybe" "Just" p h
    where (p, h)    = analyzeWithHeight x

instance (Analyze a, Analyze b) => Analyze (Either a b) where analyze = \case Left x -> let (p, h) = analyzeWithHeight x in Instance "Either" "Left" p h; Right x -> let (p, h) = analyzeWithHeight x in Instance "Either" "Right" p h
instance (Analyze a, Analyze b) => Analyze (a, b) where analyze (l, r) = let (p, h) = analyzeWithHeight2 l r in Instance "(,)" "," p h

cost :: Eq l => Op l -> Int
cost (Ins _) = 1
cost (Del _) = 1
cost (Rep f t) | f == t = 0
               | otherwise = 2

customTreeDist :: Eq l => Tree l -> Tree l -> Int
customTreeDist x1 x2 = treeDist x1 x2 cost

-- | baseline: select start element x, then sort w.r.t. x
sortWithTreeDist1 :: (Analyze a, Show a, Eq a) => [a] -> [(a, Int)]
sortWithTreeDist1 xs =
    let
      xs' = map (\x -> (x, analyze x)) xs
      s   = traceShowId $ minimumBy (comparing (height . snd)) xs'
      r   = map (second (customTreeDist (convertDataAnalysis $ snd s) . convertDataAnalysis)) xs'
    in
      sortOn (Down . snd) r

-- | variant-2: select start x, then select the next element iteratively (s.t. maximize the distance sum)
-- result = [x, y, z] (sorted)
-- xs = [a, b] -> compute [[(a, x), (a, y), ...], [(b, x), (b, y), ...]] -> map maximum (a) --> result = [x, y, z, a]
sortWithTreeDist2 :: (Analyze a, Show a, Eq a) => [a] -> [(a, Int)]
sortWithTreeDist2 xs =
    let
      r = [(minimumBy (comparing (height . analyze)) xs, 0)]
      -- xs' = xs \\ [fst $ head r]
    in
      view _1 $ foldl step (r, xs) [0..length xs - 1]
  where
    step :: (Analyze a, Show a, Eq a) => ([(a, Int)], [(a)]) -> Int -> ([(a, Int)], [a])
    step (r, s) _ =
      let
        s'  = map (\x -> (x, map (compareTwo x . fst) r)) s
        s'' = map (second sum) s'
        p   = maximumBy (comparing snd) s''
      in
        (r ++ [p], s \\ [fst p])

    compareTwo :: Analyze a => a -> a -> Int
    compareTwo a b = customTreeDist (convertDataAnalysis $ analyze a) (convertDataAnalysis $ analyze b)

-- | variant-3: select start x, then select the next element iteratively (s.t. maximize the minimum distance)
-- result = [x, y, z] (sorted)
-- xs = [a, b] -> compute [[(a, x), (a, y), ...], [(b, x), (b, y), ...]] -> map minimum --> pick the element with maximum edit distance --> result = [x, y, z, a]
-- x should have the max distance among all possible results
sortWithTreeDist3 :: (Analyze a, Show a, Eq a) => [a] -> [(a, Int)]
sortWithTreeDist3 xs =
    let
      r = [(minimumBy (comparing (height . analyze)) xs, 0)]
      -- xs' = xs \\ [fst $ head r]
    in
      view _1 $ foldl step (r, xs) [0..length xs - 1]
  where
    step :: (Analyze a, Show a, Eq a) => ([(a, Int)], [(a)]) -> Int -> ([(a, Int)], [a])
    step (r, s) _ =
      let
        s'  = map (\x -> (x, map (compareTwo x . fst) r)) s
        s'' = map (second minimum) s'
        p   = maximumBy (comparing snd) s''
      in
        (r ++ [p], s \\ [fst p])

    compareTwo :: Analyze a => a -> a -> Int
    compareTwo a b = customTreeDist (convertDataAnalysis $ analyze a) (convertDataAnalysis $ analyze b)

elementwiseSum :: Num a => [(a, a)] -> (a, a)
elementwiseSum xs = bimap sum sum $ unzip xs

sort :: (Analyze a, Show a, Eq a) => [a] -> Int -> [a]
sort [] _ = []
sort xs depth = view _1 $ foldl g (traceShowId [minimumBy (comparing (height . analyze)) xs], xs) [0..depth]
  where
    g :: (Analyze a, Show a, Eq a) => ([a], [a]) -> Int -> ([a], [a])
    g (r, s) d =
                  let s'  = map (\x' -> (map (\x -> compareData (analyze x) (analyze x') d True) r, x')) s
                      s'' = (map (first elementwiseSum) $ filter (not . any isNoDiff . fst) s')
                      p   = map last $ groupOn fst s''
                      p'  = traceShowId (map snd $ sortOn fst p)
                  in  (r ++ p', s \\ p')



    groupOn :: (Ord b) => (a -> b) -> [a] -> [[a]]
    groupOn f =
      let unpack = fmap snd . Map.toList
          fld m a = case Map.lookup (f a) m of
            Nothing -> Map.insert (f a) [a] m
            Just as -> Map.insert (f a) (a:as) m
      in unpack . foldl fld Map.empty

    isNoDiff :: (Int, Int) -> Bool
    isNoDiff = (==) (0, 0)


compareData :: DataAnalysis -> DataAnalysis -> Int -> Bool -> (Int, Int)
compareData (Instance xt xc xps _) (Instance _ yc yps _) depth isRecType = case depth of
        0 -> createResult (if xc == yc then 0 else 1) isRecType
        n -> if xc /= yc
            then createResult 1 isRecType
            else elementwiseSum $ zipWith (\x y -> compareData x y (depth - 1) (testRecType xt x)) xps yps

    where
        createResult r isRecType = if isRecType then (r, 0) else (0, r)
        testRecType parentTypeName (Instance t _ _ _) = t == parentTypeName

sortWithTreeDist :: (Analyze a, Show a, Eq a) => [a] -> [(a, Int)]
sortWithTreeDist = sortWithTreeDist2

main_ = do
    -- qcLists <- QC.generate QC.arbitrary :: IO [Maybe [Int]]
    qcLists <- return $ list 4 series :: IO [Maybe [Int]]
    -- scLists <- return $ list 5 series :: IO [Either Bool Int]

    r <- evaluate $ force $ sortWithTreeDist qcLists

    putStrLn (printf "#sorted - #original = %d" (length r - length qcLists))
    -- print ( analyze (Right (-24) :: Either Bool Int))
    -- print (convertDataAnalysis $ analyze (Left False :: Either Bool Int))

    putStrLn (printf "Input: \t%s" (show qcLists))
    putStrLn (printf "Result: %s" (show r))