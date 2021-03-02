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

import Test.SmallCheck
import Test.SmallCheck.Series
import Data.List (sortOn, (\\), maximumBy, minimumBy)
import qualified Data.Map as Map
import Data.Bifunctor
import Debug.Trace
import Control.Lens
import Data.Ord

data DataAnalysis =
  Instance  { typeName          :: String
            , constructorName   :: String
            , parameters        :: [DataAnalysis]
            , height            :: Int
            } deriving (Show, Eq)

analyzeWithHeight :: (Analyze a) => a -> ([DataAnalysis], Int)
analyzeWithHeight a =
    let instances = [analyze a] in
        (instances, 1 + (maximum $ map height instances))

analyzeWithHeight2 :: (Analyze a, Analyze b) => a -> b -> ([DataAnalysis], Int)
analyzeWithHeight2 a b =
    let instances = [analyze a, analyze b] in
        (instances, 1 + (maximum $ map height instances))

class Analyze a where analyze :: a -> DataAnalysis
instance Analyze Int where analyze x = Instance "Int" (if x == 0 then "Z" else if x > 0 then "P" else "N") [] 0
instance Analyze Bool where analyze x = Instance "Bool" (show x) [] 0
instance Analyze Char where analyze x = Instance "Char" "_" [] 0
instance Analyze a => Analyze [a] where
    analyze = \case [] -> Instance "List" "Nil" [] 0; x:xs -> let (p, h) = analyzeWithHeight2 x xs in Instance "List" "Cons" p h
instance Analyze a => Analyze (Maybe a) where
    analyze = \case Nothing -> Instance "Maybe" "Nothing" [] 0; Just x -> let (p, h) = analyzeWithHeight x in Instance "Maybe" "Just" p h
instance (Analyze a, Analyze b) => Analyze (Either a b) where
    analyze = \case Left x -> let (p, h) = analyzeWithHeight x in Instance "Either" "Left" p h; Right x -> let (p, h) = analyzeWithHeight [x] in Instance "Either" "Right" p h
instance (Analyze a, Analyze b) => Analyze (a, b) where
    analyze (l, r) = let (p, h) = analyzeWithHeight2 l r in Instance "(,)" "," p h

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

main = do
    qcLists <- QC.generate (QC.arbitrary) :: IO [(Bool, Int)]
    -- scLists <- return $ list 5 series :: IO [Either Bool Int]

    r <- evaluate $ force $ sort qcLists 5

    putStrLn (printf "Input: \t%s" (show qcLists))
    putStrLn (printf "Result: %s" (show r))