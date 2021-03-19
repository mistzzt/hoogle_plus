{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable, LambdaCase, DeriveGeneric, DeriveAnyClass #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
module InternalTypeGen where

import GHC.Generics (Generic)
import Control.DeepSeq (force, NFData(..))
import Control.Exception (evaluate)
import Control.Monad (liftM2)
import Control.Monad.Logic (liftM)
import Data.Char (ord)
import Data.Containers.ListUtils (nubOrd)
import Data.Data (Data(..))
import Data.IORef (readIORef, newIORef, modifyIORef', IORef(..))
import Data.List (isInfixOf, elemIndex, nub, drop, reverse, intersect, intercalate)
import Text.Printf (printf)
import qualified Test.ChasingBottoms as CB
import qualified Test.QuickCheck as QC

-- import Test.QuickCheck
-- import Test.QuickCheck.Property
-- import Test.QuickCheck.Monadic

defaultMaxOutputLength    = 50         :: CB.Nat
defaultTimeoutMicro       = 400         :: Int
defaultIntRange           = [-2..10]    :: [Int]
defaultCharRange          = ['a'..'d']  :: [Char]
defaultFuncSpecialSize    = 4           :: Int
defaultTestArgs           = QC.stdArgs {QC.chatty = False, QC.maxDiscardRatio = 1, QC.maxSuccess = 100, QC.maxSize = 7} :: QC.Args

newtype InternalExample = InternalExample [DataAnalysis] deriving (Show)

data DataAnalysis =
  Instance  { typeName          :: String
            , constructorName   :: String
            , expr              :: String
            , parameters        :: [DataAnalysis]
            , height            :: Int
            } deriving (Show, Eq, Generic, NFData)

instance Eq a => Eq (CB.Result a) where
  (CB.Value a) == (CB.Value b) = a == b
  CB.NonTermination == CB.NonTermination = True
  (CB.Exception _) == (CB.Exception _) = True
  _ == _ = False

instance Ord a => Ord (CB.Result a) where
  (CB.Value a) `compare` (CB.Value b) = a `compare` b
  (CB.Value _) `compare` _ = GT
  (CB.Exception _) `compare` (CB.Exception _) = EQ
  (CB.Exception _) `compare` (CB.Value _) = LT
  (CB.Exception _) `compare` _ = GT
  (CB.NonTermination) `compare` (CB.NonTermination) = EQ
  (CB.NonTermination) `compare` _ = LT

isFailedResult :: CB.Result String -> Bool
isFailedResult result = case result of
  CB.NonTermination -> True
  CB.Exception _ -> True
  CB.Value a | "_|_" `isInfixOf` a -> True
  CB.Value a | "Exception" `isInfixOf` a -> True
  _ -> False

anyDuplicate :: Ord a => [a] -> Bool
anyDuplicate [x, y] = x == y
anyDuplicate xs = length (nubOrd xs) /= length xs

analyzeTop :: (Show a, Analyze a) => a -> DataAnalysis
analyzeTop x = (analyze x) {expr = show x}

storeEval :: (Data a, Analyze a, QC.Testable prop) => IORef [[InternalExample]] -> [DataAnalysis] -> [a] -> ([CB.Result String] -> prop) -> IO QC.Property
storeEval storeRef inputs values prop = do
    outputs <- mapM (liftM splitResult . evaluateValue defaultTimeoutMicro) values

    let examples = map (\(expr, analysis) -> InternalExample (inputs ++ [(convertCBAnalysis analysis) {expr = showCBResult expr}])) outputs
    modifyIORef' storeRef (\xss -> examples : xss)

    return (QC.property $ prop $ map fst outputs)
  where
    evaluateValue :: (Data a, Analyze a) => Int -> a -> IO (CB.Result (String, DataAnalysis))
    evaluateValue timeInMicro x = CB.timeOutMicro timeInMicro $ liftM2 (,) (t x) (s x)
      where
        t = evaluate . force . CB.approxShow defaultMaxOutputLength
        s = evaluate . force . analyze -- evaluate only evaluates to weak head normal form

    splitResult :: CB.Result (a, b) -> (CB.Result a, CB.Result b)
    splitResult = \case
      CB.Value (a, b) -> (CB.Value a, CB.Value b)
      CB.NonTermination -> (CB.NonTermination, CB.NonTermination)
      CB.Exception ex -> (CB.Exception ex, CB.Exception ex)

    showCBResult :: CB.Result String -> String
    showCBResult = \case
                      CB.Value a | "_|_" `isInfixOf` a -> "bottom"
                      CB.Value a -> a
                      CB.NonTermination -> "diverge"
                      CB.Exception ex -> show ex

    convertCBAnalysis :: CB.Result DataAnalysis -> DataAnalysis
    convertCBAnalysis = \case CB.Value a -> a; _ -> Instance "error" "DNE" "" [] 0

-- * Custom Datatype for Range Restriction
newtype  MyInt = MyIntValue Int deriving (Eq, Data)
instance Ord              MyInt where compare (MyIntValue l) (MyIntValue r) = compare l r      
instance Show             MyInt where show (MyIntValue v) = show v
instance QC.Arbitrary     MyInt where arbitrary = QC.elements (map MyIntValue defaultIntRange)
instance QC.CoArbitrary   MyInt where coarbitrary (MyIntValue v) = QC.coarbitraryIntegral v

newtype  MyChar = MyCharValue Char deriving (Eq, Data)
instance Ord              MyChar where compare (MyCharValue l) (MyCharValue r) = compare l r
instance Show             MyChar where show (MyCharValue v) = show v; showList = showList . (unwrap :: [MyChar] -> [Char])
instance QC.Arbitrary     MyChar where arbitrary = QC.elements (map MyCharValue defaultCharRange)
instance QC.CoArbitrary   MyChar where coarbitrary (MyCharValue v) = QC.coarbitrary $ ord v

data     MyFun a b = Generated (a -> b) | Expression String (a -> b)
instance (QC.Arbitrary a, QC.CoArbitrary b)                       => QC.CoArbitrary (MyFun a b)   where coarbitrary = \case Generated f -> QC.coarbitrary f; Expression _ f -> QC.coarbitrary f
instance Show a                                                   => Show (MyFun a b)             where show = \case Expression str _ -> "(" ++ str ++ ")"; Generated f -> "<Generated>"
instance {-# OVERLAPPABLE #-} (QC.CoArbitrary a, QC.Arbitrary b)  => QC.Arbitrary (MyFun a b)     where arbitrary = liftM Generated QC.arbitrary

newtype  Box a            =   BoxValue a              deriving (Eq, Data)
instance Ord a            =>  Ord (Box a)             where compare (BoxValue l) (BoxValue r) = compare l r
instance Show a           =>  Show (Box a)            where show (BoxValue v) = show v
instance QC.Arbitrary a   =>  QC.Arbitrary (Box a)    where arbitrary = fmap BoxValue QC.arbitrary
instance QC.CoArbitrary a =>  QC.CoArbitrary (Box a)  where coarbitrary (BoxValue v) = QC.coarbitrary v
        
-- * Custom Datatype Conversion
class    Unwrappable a b                                                            where unwrap :: a -> b; wrap :: b -> a
instance Unwrappable a b => Unwrappable (Box a) b                                   where unwrap = unwrap . (\(BoxValue v) -> v); wrap = BoxValue . wrap
instance Unwrappable MyInt Int                                                      where unwrap (MyIntValue v) = v; wrap = MyIntValue
instance Unwrappable MyChar Char                                                    where unwrap (MyCharValue v) = v; wrap = MyCharValue
instance (Unwrappable a c, Unwrappable b d)   => Unwrappable (a -> b)    (c -> d)   where unwrap f = \x -> unwrap $ f $ wrap x; wrap f = \x -> wrap $ f $ unwrap x
instance (Unwrappable a c, Unwrappable b d)   => Unwrappable (MyFun a b) (c -> d)   where unwrap (Generated f) = unwrap f; unwrap (Expression _ f) = unwrap f; wrap f = Generated (wrap f)

instance                                         Unwrappable a a                    where unwrap = id; wrap = id
instance {-# OVERLAPPING #-} Unwrappable a b  => Unwrappable [a] [b]                where unwrap = fmap unwrap; wrap = fmap wrap
instance {-# OVERLAPPING #-} Unwrappable a b  => Unwrappable (Maybe a) (Maybe b)    where unwrap = fmap unwrap; wrap = fmap wrap
instance (Unwrappable a c, Unwrappable b d)   => Unwrappable (a, b) (c, d)          where unwrap (x, y) = (unwrap x, unwrap y); wrap (x, y) = (wrap x, wrap y)

instance (Unwrappable a c, Unwrappable b d)   => Unwrappable (Either a b) (Either c d) where
  wrap    = \case Left v -> Left $ wrap v;    Right v -> Right $ wrap v
  unwrap  = \case Left v -> Left $ unwrap v;  Right v -> Right $ unwrap v


-- * Data Analysis
class                                       AnalyzeManyType r               where amImpl :: [DataAnalysis] -> r
instance                                    AnalyzeManyType [DataAnalysis]  where amImpl = id
instance (Analyze a, AnalyzeManyType b) =>  AnalyzeManyType (a -> b)        where amImpl acc = amImpl . (acc ++) . (:[]) . analyze

analyzeMany :: (AnalyzeManyType r) => r
analyzeMany = amImpl []

createInstance :: String -> String -> [DataAnalysis] -> DataAnalysis
createInstance typeName constrName params = Instance typeName constrName "" params (maybe 0 (+1) (foldr max Nothing $ map (Just . height) params))

class                               Analyze a             where analyze :: a -> DataAnalysis
instance                            Analyze Int           where analyze x = Instance "Int"  (show $ x `compare` 0) (show x) [] 0
instance                            Analyze Bool          where analyze x = Instance "Bool" (show x)               (show x) [] 0
instance                            Analyze Char          where analyze x = Instance "Char" "_"                    (show x) [] 0
instance                            Analyze MyInt         where analyze = analyze . (unwrap :: MyInt -> Int)
instance                            Analyze MyChar        where analyze = analyze . (unwrap :: MyChar -> Char)
instance                            Analyze (a -> b)      where analyze = const (Instance "Fun" "_" "" [] 0)
instance                            Analyze (MyFun a b)   where analyze = \case Generated _ -> Instance "Fun" "_" "" [] 0; Expression n _ -> Instance "Fun" n "" [] 0
instance Show a                 =>  Analyze (Box a)       where analyze x = Instance "Box" "_" (show x) [] 0
instance Analyze a              =>  Analyze [a]           where analyze = \case [] -> createInstance "List" "Nil" []; x:xs -> createInstance "List" "Cons" (analyzeMany x xs)
instance Analyze a              =>  Analyze (Maybe a)     where analyze = maybe (createInstance "Maybe" "Nothing" []) (createInstance "Maybe" "Just" . analyzeMany)
instance (Analyze a, Analyze b) =>  Analyze (Either a b)  where analyze = either (createInstance "Either" "Left" . analyzeMany) (createInstance "Either" "Right" . analyzeMany)
instance (Analyze a, Analyze b) =>  Analyze (a, b)        where analyze (l, r) = createInstance "(,)" "," (analyzeMany l r)