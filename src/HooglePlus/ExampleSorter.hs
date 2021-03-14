{-# LANGUAGE FlexibleContexts #-}
module HooglePlus.ExampleSorter where

import Data.List (sortOn, sortBy, (\\), maximumBy, minimumBy, foldl')
import qualified Data.Map as Map
import Data.Bifunctor
import Control.Lens
import Data.Ord

import TED.Data.Tree.Diff (n, treeDist, simpleTreeDist, Op(..))
import TED.Data.Tree (Tree(..))
import Types.Filtering

import Debug.Trace

import DraftSort

sortWithTreeDistVar :: [DataAnalysis] -> [(DataAnalysis, Int)]
sortWithTreeDistVar xs =
    let
      r = [(minimumBy (comparing height) xs, 0)]
    in
      view _1 $ foldl' step (r, xs) [0..length xs - 1]
  where
    step :: ([(DataAnalysis, Int)], [DataAnalysis]) -> Int -> ([(DataAnalysis, Int)], [DataAnalysis])
    step (r, s) _ =
      let
        s'  = map (\x -> (x, map (compareTwo x . fst) r)) s
        s'' = map (second minimum) s'
        p   = maximumBy (comparing snd) s''
      in
        (r ++ [p], s \\ [fst p])

    compareTwo :: DataAnalysis -> DataAnalysis -> Int
    compareTwo a b = customTreeDist (convertDataAnalysis a) (convertDataAnalysis b)