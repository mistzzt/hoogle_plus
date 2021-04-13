{-# LANGUAGE FlexibleContexts, TupleSections #-}
module HooglePlus.ExampleSorter (sortWithTreeDistVar) where

import Control.Lens ( view, Field1(_1), Field2(_2), Field3(_3) )
import Control.Monad.State ( foldM, modify, evalState, MonadState(get), StateT )
import Data.Bifunctor ( Bifunctor(second) )
import Data.Functor
import Data.Hashable ( Hashable(hash) )
import Data.List (sortOn, sortBy, (\\), maximumBy, minimumBy, foldl', iterate', sort, unfoldr)
import Data.List.Extra (groupOn)
import Data.Ord ( comparing )
import qualified Data.Map as Map
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomR, getStdGen, RandomGen)

import Types.Filtering ( DataAnalysis(height, parameters) )
import TED.Data.Tree.Diff (n, treeDist, simpleTreeDist, Op(..))
import TED.Data.Tree (Tree(..))
import DraftSort ( customTreeDist, convertDataAnalysis )

import Debug.Trace

type ExampleSorter m = (DataAnalysis -> DataAnalysis) -> [DataAnalysis] -> CachedTed m [(DataAnalysis, Int)]
type TierExampleSorter m = (DataAnalysis -> DataAnalysis) -> [DataAnalysis] -> CachedTed m [[DataAnalysis]]
type CachedTedState = Map.Map (Int, Int) Int
type CachedTed m = StateT CachedTedState m

compareDataAnalyses :: Monad m => (DataAnalysis -> DataAnalysis) -> DataAnalysis -> DataAnalysis -> CachedTed m Int
compareDataAnalyses f a b = do
    cache <- get
    let (a', b') = (f a, f b)
    let [hashL, hashR] = sort [hash a', hash b']
    let key = (hashL, hashR)
    case Map.lookup key cache of
      Just v -> return v
      Nothing -> do
        let v = customTreeDist (convertDataAnalysis a') (convertDataAnalysis b')
        modify $ \c -> Map.insert key v c
        return v

subsetSize :: Int
subsetSize = 10

sortWithTreeDistVar :: [DataAnalysis] -> ([DataAnalysis], [[DataAnalysis]])
sortWithTreeDistVar xs = evalState (work xs) Map.empty

sortWithTreeDistVar_ :: Monad m => [DataAnalysis] -> CachedTed m [(DataAnalysis, Int)]
sortWithTreeDistVar_ = simpleSortBy id
-- simpleSortBy id
-- stochasticSimpleSortBy id
-- cherryPickByOutput simpleSortBy
-- cherryPickByOutput stochasticSimpleSortBy

work :: Monad m => [DataAnalysis] -> CachedTed m ([DataAnalysis], [[DataAnalysis]])
-- work xs = previewSimilarExamples (stochasticSimpleSortBy id) xs 10
work xs = (\x -> (x, map (const []) x)) <$> cherryPickByOutput stochasticSimpleSortBy tierSortBy xs -- previewSimilarExamples sortWithTreeDistVar_ xs 10

previewSimilarExamples :: Monad m => ([DataAnalysis] -> CachedTed m [(DataAnalysis, Int)]) -> [DataAnalysis] -> Int -> CachedTed m ([DataAnalysis], [[DataAnalysis]])
previewSimilarExamples f xs n = do
    sorted <- take n . map fst <$> f xs
    mapM (`findSimilar` xs) sorted <&> (sorted,)
  where
    findSimilar :: Monad m => DataAnalysis -> [DataAnalysis] -> CachedTed m [DataAnalysis]
    findSimilar x xs = take n . map fst . sortOn snd <$> mapM (\e -> (e,) <$> compareDataAnalyses id x e) (xs \\ [x])


simpleSortBy :: Monad m => ExampleSorter m
simpleSortBy f xs =
    let
      r = [(minimumBy (comparing height) xs, 0)]
    in
      view _1 <$> foldM (step id) (r, xs) [0..length xs - 1]
  where
    step :: Monad m => (DataAnalysis -> DataAnalysis) -> ([(DataAnalysis, Int)], [DataAnalysis]) -> Int -> CachedTed m ([(DataAnalysis, Int)], [DataAnalysis])
    step f (r, s) _ = do
      s' <- mapM (\x -> fmap (x,) (mapM (compareDataAnalyses f x . fst) r)) s
      let s'' = map (second minimum) s'
      let p   = maximumBy (comparing snd) s''
      return (r ++ [p], s \\ [fst p])

tierSortBy :: Monad m => TierExampleSorter m
tierSortBy f xs = do
    let r = minimumBy (comparing height) xs
    map (map fst) . groupOn snd . sortOn snd <$> mapM (\x -> fmap (x,) (compareDataAnalyses f r x)) xs

cherryPickByOutput :: Monad m => ExampleSorter m -> TierExampleSorter m -> [DataAnalysis] -> CachedTed m [DataAnalysis]
cherryPickByOutput g f xs = do
    tiers <- f (last . parameters) xs
    mapM (fmap (head . map fst). g (\x -> x {parameters = (init . parameters) x})) tiers

stochasticSimpleSortBy :: Monad m => ExampleSorter m
stochasticSimpleSortBy f xs =
    let
      r = [(minimumBy (comparing height) xs, 0)]
    in
      view _1 <$> foldM (step id) (r, xs, unsafePerformIO getStdGen) [0..length xs - 1]
  where
    step :: (RandomGen g, Monad m) => (DataAnalysis -> DataAnalysis) -> ([(DataAnalysis, Int)], [DataAnalysis], g) -> Int -> CachedTed m ([(DataAnalysis, Int)], [DataAnalysis], g)
    step f (r, s, gen) _ = do
      let (idxs, gen') = rolls (0, length s - 1) subsetSize gen
      s' <- mapM ((\x -> fmap (x,) (mapM (compareDataAnalyses f x . fst) r)) . (!!) s) idxs
      let p  =  maximumBy (comparing snd) $ map (second minimum) s'
      return (r ++ [p], s \\ [fst p], gen')

    rolls :: RandomGen g => (Int, Int) -> Int -> g -> ([Int], g)
    rolls r n g = iterate' (\(idxs, gen) -> let (i, gen') = randomR r gen in (i:idxs, gen') ) ([], g) !! n

-- sortWithTreeDistVarStochOnResult :: [DataAnalysis] -> [(DataAnalysis, Int)]
-- sortWithTreeDistVarStochOnResult xs =
--     let
--       r = [(minimumBy (comparing height) xs, 0)]
--     in
--       view _1 $ foldl' step (r, xs, unsafePerformIO getStdGen) [0..length xs - 1]
--   where
--     -- r: result; xs: input set
--     step :: RandomGen g => ([(DataAnalysis, Int)], [DataAnalysis], g) -> Int -> ([(DataAnalysis, Int)], [DataAnalysis], g)
--     step (r, s, gen) _ =
--       let
--         (idxs, gen')  = iterate' (\(idxs, gen) -> let (i, gen') = randomR (0 :: Int, length r - 1 :: Int) gen in (i:idxs, gen') ) ([], gen) !! subsetSize
--         r'            = map (fst . (!!) r) idxs
--         s'            = map (\x -> (x, map (compareDataAnalyses id x) r')) s
--         s'' = map (second minimum) s'
--         p   = maximumBy (comparing snd) s''
--       in
--         (r ++ [p], s \\ [fst p], gen')