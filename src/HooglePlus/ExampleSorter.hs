{-# LANGUAGE FlexibleContexts #-}
module HooglePlus.ExampleSorter where

import Data.List (sortOn, sortBy, (\\), maximumBy, minimumBy, foldl', iterate')
import qualified Data.Map as Map
import Data.Bifunctor ( Bifunctor(second) )
import Control.Lens ( view, Field1(_1) )
import Data.Ord ( comparing )

import System.Random (randomR, getStdGen, RandomGen)
import System.IO.Unsafe (unsafePerformIO)

import Types.Filtering ( DataAnalysis(height) )
import TED.Data.Tree.Diff (n, treeDist, simpleTreeDist, Op(..))
import TED.Data.Tree (Tree(..))
import DraftSort ( customTreeDist, convertDataAnalysis )

import Debug.Trace

sortWithTreeDistVar :: [DataAnalysis] -> [(DataAnalysis, Int)]
sortWithTreeDistVar xs =
    let
      r = [(minimumBy (comparing height) xs, 0)]
    in
      view _1 $ foldl' step (r, xs, unsafePerformIO getStdGen) [0..length xs - 1]
  where
    -- r: result; xs: input set
    step :: RandomGen g => ([(DataAnalysis, Int)], [DataAnalysis], g) -> Int -> ([(DataAnalysis, Int)], [DataAnalysis], g)
    step (r, s, gen) _ =
      let
        (idxs, gen')  = iterate' (\(idxs, gen) -> let (i, gen') = randomR (0 :: Int, length s - 1 :: Int) gen in (i:idxs, gen') ) ([], gen) !! 10
        s'            = map ((\x -> (x, map (compareTwo x . fst) r)) . (!!) s) idxs
        s'' = map (second minimum) s'
        p   = maximumBy (comparing snd) s''
      in
        (r ++ [p], s \\ [fst p], gen')

    compareTwo :: DataAnalysis -> DataAnalysis -> Int
    compareTwo a b = customTreeDist (convertDataAnalysis a) (convertDataAnalysis b)