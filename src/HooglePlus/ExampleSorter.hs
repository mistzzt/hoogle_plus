{-# LANGUAGE FlexibleContexts #-}
module HooglePlus.ExampleSorter (sortWithTreeDistVar) where

import Data.List (sortOn, sortBy, (\\), maximumBy, minimumBy, foldl', iterate')
import qualified Data.Map as Map
import Data.Bifunctor ( Bifunctor(second) )
import Control.Lens ( view, Field1(_1) )
import Data.Ord ( comparing )

import System.Random (randomR, getStdGen, RandomGen)
import System.IO.Unsafe (unsafePerformIO)

import Types.Filtering ( DataAnalysis(height, parameters) )
import TED.Data.Tree.Diff (n, treeDist, simpleTreeDist, Op(..))
import TED.Data.Tree (Tree(..))
import DraftSort ( customTreeDist, convertDataAnalysis )

import Debug.Trace

type ExampleSorter = (DataAnalysis -> DataAnalysis) -> [DataAnalysis] -> [(DataAnalysis, Int)]

compareDataAnalyses :: (DataAnalysis -> DataAnalysis) -> DataAnalysis -> DataAnalysis -> Int
compareDataAnalyses f a b = customTreeDist (convertDataAnalysis $ f a) (convertDataAnalysis $ f b)

subsetSize :: Int
subsetSize = 10

sortWithTreeDistVar :: [DataAnalysis] -> [(DataAnalysis, Int)]
sortWithTreeDistVar = cherryPickByOutput stochasticSimpleSortBy
-- simpleSortBy id
-- stochasticSimpleSortBy id
-- cherryPickByOutput simpleSortBy
-- cherryPickByOutput stochasticSimpleSortBy

simpleSortBy :: ExampleSorter
simpleSortBy f xs =
    let
      r = [(minimumBy (comparing height) xs, 0)]
    in
      view _1 $ foldl' (step id) (r, xs) [0..length xs - 1]
  where
    step :: (DataAnalysis -> DataAnalysis) -> ([(DataAnalysis, Int)], [DataAnalysis]) -> Int -> ([(DataAnalysis, Int)], [DataAnalysis])
    step f (r, s) _ =
      let
        s'  = map (\x -> (x, map (compareDataAnalyses f x . fst) r)) s
        s'' = map (second minimum) s'
        p   = maximumBy (comparing snd) s''
      in
        (r ++ [p], s \\ [fst p])

stochasticSimpleSortBy :: ExampleSorter
stochasticSimpleSortBy f xs =
    let
      r = [(minimumBy (comparing height) xs, 0)]
    in
      view _1 $ foldl' (step id) (r, xs, unsafePerformIO getStdGen) [0..length xs - 1]
  where
    step :: RandomGen g => (DataAnalysis -> DataAnalysis) -> ([(DataAnalysis, Int)], [DataAnalysis], g) -> Int -> ([(DataAnalysis, Int)], [DataAnalysis], g)
    step f (r, s, gen) _ =
      let
        (idxs, gen')  = iterate' (\(idxs, gen) -> let (i, gen') = randomR (0 :: Int, length s - 1 :: Int) gen in (i:idxs, gen') ) ([], gen) !! subsetSize
        s'            = map ((\x -> (x, map (compareDataAnalyses f x . fst) r)) . (!!) s) idxs
        s''           = map (second minimum) s'
        p             = maximumBy (comparing snd) s''
      in
        (r ++ [p], s \\ [fst p], gen')

cherryPickByOutput :: ExampleSorter -> [DataAnalysis] -> [(DataAnalysis, Int)]
cherryPickByOutput f xs =
    let
      pickByOutput = (map (view _1) . take 10 . f (last . parameters)) xs
    in
      f (\x -> x {parameters = (init . parameters) x}) pickByOutput

sortWithTreeDistVarStochOnResult :: [DataAnalysis] -> [(DataAnalysis, Int)]
sortWithTreeDistVarStochOnResult xs =
    let
      r = [(minimumBy (comparing height) xs, 0)]
    in
      view _1 $ foldl' step (r, xs, unsafePerformIO getStdGen) [0..length xs - 1]
  where
    -- r: result; xs: input set
    step :: RandomGen g => ([(DataAnalysis, Int)], [DataAnalysis], g) -> Int -> ([(DataAnalysis, Int)], [DataAnalysis], g)
    step (r, s, gen) _ =
      let
        (idxs, gen')  = iterate' (\(idxs, gen) -> let (i, gen') = randomR (0 :: Int, length r - 1 :: Int) gen in (i:idxs, gen') ) ([], gen) !! subsetSize
        r'            = map (fst . (!!) r) idxs
        s'            = map (\x -> (x, map (compareDataAnalyses id x) r')) s
        s'' = map (second minimum) s'
        p   = maximumBy (comparing snd) s''
      in
        (r ++ [p], s \\ [fst p], gen')