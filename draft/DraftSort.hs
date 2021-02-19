{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable, LambdaCase #-}
module Sort where

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

import Data.List (sortOn, (\\))
import qualified Data.Map as Map
import Data.Bifunctor

type ConstructorName = String
type TypeName = String
data DataAnalysis = Instance TypeName ConstructorName [DataAnalysis] deriving (Show, Eq)

class Analyze a where analyze :: a -> DataAnalysis
instance Analyze Int where analyze x = Instance "Int" (if x == 0 then "Z" else if x > 0 then "P" else "N") []
instance Analyze Bool where analyze x = Instance "Bool" (show x) []
instance Analyze Char where analyze x = Instance "Char" "_" []
instance Analyze a => Analyze [a] where
    analyze = \case [] -> Instance "List" "Nil" []; x:xs -> Instance "List" "Cons" [analyze x, analyze xs]
instance Analyze a => Analyze (Maybe a) where
    analyze = \case Nothing -> Instance "Maybe" "Nothing" []; Just x -> Instance "Maybe" "Just" [analyze x]
instance (Analyze a, Analyze b) => Analyze (Either a b) where
    analyze = \case Left x -> Instance "Either" "Left" [analyze x]; Right x -> Instance "Either" "Right" [analyze x]

sort :: (Analyze a, Show a, Eq a) => [a] -> Int -> [a]
sort [] _ = []
sort (x:xs) depth = fst $ foldl f ([], xs) [0..depth]
  where
    xd = analyze x
    f (r, s) d = 
                  let s'  = map (\x' -> (compareData xd (analyze x') d True, x')) s
                      p   = map head $ groupOn fst s'
                      p'  = map snd $ sortOn fst p
                      r'  = p' ++ r
                      s'' = s \\ p'
                  in (r', s'')

    groupOn :: (Ord b) => (a -> b) -> [a] -> [[a]]
    groupOn f =
      let unpack = fmap snd . Map.toList
          fld m a = case Map.lookup (f a) m of
            Nothing -> Map.insert (f a) [a] m
            Just as -> Map.insert (f a) (a:as) m
      in unpack . foldl fld Map.empty


compareData :: DataAnalysis -> DataAnalysis -> Int -> Bool -> (Int, Int)
compareData (Instance xt xc xps) (Instance _ yc yps) depth isRecType = case depth of
        0 -> createResult (if xc == yc then 0 else 1) isRecType
        n -> if xc /= yc
            then createResult 1 isRecType
            else elementwiseSum $ zipWith (\x y -> compareData x y (depth - 1) (testRecType xt x)) xps yps
            
    where
        createResult r isRecType = if isRecType then (r, 0) else (0, r)
        testRecType parentTypeName (Instance t _ _) = t == parentTypeName
        elementwiseSum xs = bimap sum sum $ unzip xs

l1 = analyze ((Left $ Left 0) :: Either (Either Int Int) Int)
l2 = analyze ((Left $ Right 7) :: Either (Either Int Int) Int)

test x = compareData l1 l2 x True