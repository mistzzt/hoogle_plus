{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable, DeriveGeneric, DeriveAnyClass, LambdaCase, NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module InternalTypeGen where

import GHC.Generics (Generic)
import Control.DeepSeq (force, NFData(..))
import Control.Exception (evaluate)
import Control.Monad (liftM2, when)
import Control.Monad.Logic (liftM)
import Data.Char (ord)
import Data.Data (Data(..))
import Data.IORef (readIORef, newIORef, modifyIORef', IORef(..))
import Data.List (isInfixOf, elemIndex, nub, drop, reverse, intersect, intercalate)
import Text.Printf (printf)
import qualified Test.ChasingBottoms as CB
import qualified Test.QuickCheck as QC

-- import qualified Data.Vector         as V
-- import InternalTed

-- import Test.QuickCheck
-- import Test.QuickCheck.Property
-- import Test.QuickCheck.Monadic
-- import Text.Show.Functions

-- import Data.Maybe

defaultMaxOutputLength    = 50         :: CB.Nat
defaultTimeoutMicro       = 400         :: Int
defaultIntRange           = [-2..10]    :: [Int]
defaultCharRange          = ['a'..'d']  :: [Char]
defaultFuncSpecialSize    = 4           :: Int
defaultTestArgs           = QC.stdArgs {QC.chatty = False, QC.maxDiscardRatio = 1, QC.maxSuccess = 100, QC.maxSize = 7} :: QC.Args

newtype InternalExample = InternalExample [String] deriving (Show)

instance Eq a => Eq (CB.Result a) where
  CB.Value a        == CB.Value b         = a == b
  CB.NonTermination == CB.NonTermination  = True
  CB.Exception _    == CB.Exception _     = True
  _                 == _                  = False

isFailedResult :: CB.Result String -> Bool
isFailedResult = \case
  CB.Value a
    | "_|_" `isInfixOf` a       -> True
    | "Exception" `isInfixOf` a -> True
  CB.NonTermination             -> True
  CB.Exception _                -> True
  _                             -> False

anyDuplicate :: Eq a => [a] -> Bool
anyDuplicate = \case
  [x, y] -> x == y
  xs     -> length (nub xs) /= length xs

storeEval :: Data a => IORef [[InternalExample]] -> [String] -> [a] -> ([CB.Result String] -> Bool) -> Bool -> IO QC.Property
storeEval storeRef inputs values prop includeFailed = do
    outputs <- mapM (evaluateValue defaultTimeoutMicro) values

    let examples = map (\expr -> InternalExample (inputs ++ [showCBResult expr])) outputs
    let propTest = prop outputs
    when (includeFailed || propTest)
      (modifyIORef' storeRef (\xss -> examples : xss))

    return (propTest QC.==> True)
  where
    evaluateValue :: Data a => Int -> a -> IO (CB.Result String)
    evaluateValue timeInMicro x = CB.timeOutMicro timeInMicro (t x)
      where
        t = evaluate . force . CB.approxShow defaultMaxOutputLength

    showCBResult :: CB.Result String -> String
    showCBResult = \case
                      CB.Value a | "_|_" `isInfixOf` a -> a
                      CB.Value a -> a
                      CB.NonTermination -> "diverge"
                      CB.Exception ex -> show ex

-- main_ =
--   let wrappedSolution = ((\f xs -> Data.Maybe.listToMaybe (Data.Maybe.mapMaybe f xs)) :: () => ((Int) -> (((Maybe) (String)))) -> [Int] -> ((Maybe) (String))) in
--     let executeWrapper (arg_1 :: ((Int) -> (((Maybe) (String))))) (arg_2 :: [Int]) = (Prelude.map (\f -> f (arg_1) (arg_2)) [wrappedSolution]) in
--       let prop_not_crash storeRef (arg_1) (arg_2) = monadicIO $ run $ storeEval storeRef ([(show arg_1), (show arg_2)]) (executeWrapper (arg_1) (arg_2)) (not . isFailedResult . Prelude.head) True in
--         newIORef [] >>= (\storeRef -> liftM2 (,) (quickCheckWithResult defaultTestArgs (prop_not_crash storeRef)) (readIORef storeRef))
--   let wrappedSolution = ((\f x -> f x) :: () => ((Int) -> (String)) -> Int -> String) in
--     let executeWrapper (arg_1 :: ((((MyFun) (((Box) (MyInt))))) (((Box) ([MyChar]))))) (arg_2 :: ((Box) (MyInt))) = (Prelude.map (\f -> f) [wrappedSolution]) in
--       let prop_not_crash storeRef (arg_1) (arg_2) = monadicIO $ run $ storeEval storeRef ([(show arg_1), (show arg_2)]) (executeWrapper (arg_1) (arg_2)) (not . isFailedResult . Prelude.head) True in
--         newIORef [] >>= (\storeRef -> liftM2 (,) (quickCheckWithResult defaultTestArgs (prop_not_crash storeRef)) (readIORef storeRef))