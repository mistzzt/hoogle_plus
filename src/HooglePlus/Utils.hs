{-# LANGUAGE FlexibleContexts, NamedFieldPuns, LambdaCase, TupleSections #-}

module HooglePlus.Utils where

import Types.Common
import Types.Environment
import Types.Program
import Types.Type
import Types.Experiments
import Types.Solver
import qualified Types.TypeChecker as Checker
import Types.Filtering
import Types.IOFormat (Example(Example))
import Synquid.Type
import Synquid.Utils
import Synquid.Pretty as Pretty
import Synquid.Program
import Database.Utils
import HooglePlus.ExampleSorter
import Data.Hashable ( Hashable(hash) )

import Control.Exception
import Control.Monad.Trans
import Control.Monad.State
import CorePrep
import CoreSyn
import Data.Bifunctor (first, second)
import Data.Data
import Data.Ord
import Data.Either
import Data.List (sortOn, groupBy, isInfixOf, isPrefixOf, isSuffixOf, intercalate, sort)
import Data.List.Extra (nubOrdOn, dropEnd)
import Data.List.Split (splitOn)
import Data.Maybe
import Data.Typeable
import Data.Function (on)
import Demand
import DmdAnal
import DynFlags
import FamInstEnv
import GHC hiding (Id)
import GHC.Paths ( libdir )
import HscTypes
import IdInfo
import Outputable hiding (text, (<+>))
import qualified CoreSyn as Syn
import qualified Data.Map.Strict as Map hiding (foldr)
import qualified Data.Set as Set hiding (map)
import qualified Data.Text as Text
import SimplCore (core2core)
import System.Directory (removeFile)
import Text.Printf
import Text.Regex
import Var hiding (Id)
import Data.UUID.V4
import Debug.Trace
import qualified Language.Haskell.Interpreter as LHI
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as LB

import System.Random ( Random(randomR), StdGen, getStdRandom )
import Data.Array.ST
    ( STArray, newListArray, readArray, writeArray )
import Control.Monad.ST ( ST, runST )
import Data.STRef ( newSTRef, readSTRef, writeSTRef )
import System.IO.Unsafe (unsafePerformIO)

-- Converts the list of param types into a haskell function signature.
-- Moves typeclass-looking things to the front in a context.
mkFunctionSigStr :: [TypeSkeleton] -> String
mkFunctionSigStr args = addConstraints $ Prelude.foldr accumConstraints ([],[]) args
    where
        showSigs = intercalate " -> "
        wrapParen x = "(" ++ x ++ ")"
        addConstraints ([], baseSigs) = showSigs baseSigs
        addConstraints (constraints, baseSigs) = "(" ++ (intercalate ", " constraints) ++ ") => " ++ showSigs baseSigs

        accumConstraints :: TypeSkeleton -> ([String], [String]) -> ([String], [String])
        accumConstraints (TyAppT (DatatypeT id) (TypeVarT tyvarName)) (constraints, baseSigs)
            | tyclassPrefix `isPrefixOf` id = let
                classNameRegex = mkRegex $ tyclassPrefix ++ "([a-zA-Z]*)"
                className = subRegex classNameRegex id "\\1"
                constraint = className ++ " " ++ tyvarName
                in (constraint:constraints, baseSigs)
        accumConstraints otherTy (constraints, baseSigs) = let
            otherStr = if isFunctionType otherTy then wrapParen (show otherTy) else show otherTy
            in (constraints, otherStr:baseSigs)

-- mkLambdaStr produces a oneline lambda expr str:
-- (\x y -> body))
mkLambdaStr :: [String] -> UProgram -> String
mkLambdaStr args body =
    let nontcArgs = filter (not . (tyclassArgBase `isPrefixOf`)) args
        argStr = unwords nontcArgs
        unTypeclassed = toHaskellSolution (show body)
     in printf "\\%s -> %s" argStr unTypeclassed

toHaskellSolution :: String -> String
toHaskellSolution bodyStr = let
    oneLineBody = unwords $ lines bodyStr
    noTypeclasses = removeTypeclasses oneLineBody
    in
        noTypeclasses

removeAll :: Regex -> String -> String
removeAll a b = unwords $ words $ go a b
    where
        go regex input =
            if (isJust $ matchRegex regex input)
            then (go regex $ subRegex regex input "")
            else input

removeTypeclassArgs :: String -> String
removeTypeclassArgs = removeAll (mkRegex (tyclassArgBase++"[0-9]+"))

removeTypeclassInstances :: String -> String
removeTypeclassInstances = removeAll (mkRegex (tyclassInstancePrefix ++ "[0-9]*[a-zA-Z]*"))

removeTypeclasses = removeEmptyParens . removeTypeclassArgs . removeTypeclassInstances
    where
        removeEmptyParens = removeAll (mkRegex "\\(\\ +\\)")

printSolution :: String -> IO ()
printSolution solution = do
    putStrLn "*******************SOLUTION*********************"
    putStrLn $ "SOLUTION: " ++ toHaskellSolution (show solution)
    putStrLn "************************************************"

-- | Randomly shuffle a list without the IO Monad
--   /O(N)/
-- https://wiki.haskell.org/Random_shuffle
shuffle' :: [a] -> StdGen -> ([a],StdGen)
shuffle' xs gen = runST (do
        g <- newSTRef gen
        let randomRST lohi = do
              (a,s') <- liftM (randomR lohi) (readSTRef g)
              writeSTRef g s'
              return a
        ar <- newArray n xs
        xs' <- forM [1..n] $ \i -> do
                j <- randomRST (i,n)
                vi <- readArray ar i
                vj <- readArray ar j
                writeArray ar j vi
                return vj
        gen' <- readSTRef g
        return (xs',gen'))
  where
    n = length xs
    newArray :: Int -> [a] -> ST s (STArray s Int a)
    newArray n xs =  newListArray (1,n) xs

shuffleIO :: [a] -> IO [a]
shuffleIO xs = getStdRandom (shuffle' xs)

printFilter :: String -> FilterState -> IO ()
printFilter solution fs@FilterState{discardedSolutions, solutionDescriptions, differentiateExamples} = do
        putStrLn "\n*******************FILTER*********************"
        putStrLn $ "SOLN: " ++ solution
        -- putStrLn "***IO Examples***"
        -- putStrLn $ unlines $ zipWith (\ex sms -> printf "[Example] %s\r\n%s" ex (unlines sms)) ioExamples relatedExamples
        putStrLn "***Diff Examples***"
        -- putStrLn diffExamples
        -- putStrLn $ show fs
        putStrLn $ LB.unpack $ encodeWithPrefix (experimentDataSorted, experimentDataShuffled, experimentDataRandomExamples)
        -- putStrLn "ENDED"
        putStrLn "**********************************************\n"
    where
        diffExamples = unlines $ concatMap (\(soln, examples) -> ("- " ++ soln) : map (('\t':) . show) examples) (Map.toList differentiateExamples)
        (ioExamples, relatedExamples)   = let (_, desc) = head $ filter ((== solution) . fst) solutionDescriptions in case desc of Total ex s _ -> (map show ex, map (map show) s); Partial ex s _ -> (map show ex, map (map show) s); _ -> ([], [])

        experimentDataSorted = (map (second (map (first show))) . sortExperimentData . buildExperimentData extractDescriptionPlain) fs
        experimentDataShuffled = (map (second (map (first show))) . shuffleExperimentData . buildExperimentData extractDescriptionPlain) fs
        experimentDataRandomExamples = (map (second (map (first show))) . shuffleExperimentData . buildExperimentData extractDescriptionRandom) fs

        encodeWithPrefix obj = LB.append (LB.pack "EXPRMTS:") (A.encode obj)

        buildExperimentData :: (CandidateValidDesc -> [(InternalExample, Int)]) -> FilterState -> [(String, [(InternalExample, Int)])]
        buildExperimentData f FilterState{solutionDescriptions, differentiateExamples} =
            Map.toList $
            Map.map (nubOrdOn (hash . fst)) $
                Map.unionWith (++)
                    (Map.fromList (map (second f) solutionDescriptions))
                    (Map.map (map (,1 :: Int)) differentiateExamples)

        shuffleExperimentData :: [(String, [(InternalExample, Int)])] -> [(String, [(InternalExample, Int)])]
        -- shuffleExperimentData = mapM (sequence . second (getStdRandom . shuffle'))
        shuffleExperimentData = map (second (unsafePerformIO . shuffleIO))

        sortExperimentData :: [(String, [(InternalExample, Int)])] -> [(String, [(InternalExample, Int)])]
        sortExperimentData = map (second magicSort)
            where
                magicSort :: [(InternalExample, Int)] -> [(InternalExample, Int)]
                magicSort xs =
                    let
                        keyMap = Map.fromList $ map (first hash) xs
                        queryKey ex = fromMaybe 0 $ keyMap Map.!? hash ex
                    in
                        map ((\x -> (x, queryKey x)) . unpackNodes) (sortWithTreeDistVarPlain $ map (packNodes . fst) xs)

        -- 0 plain; 1 distinguishing; 2 related (significant)
        extractDescriptionPlain :: CandidateValidDesc -> [(InternalExample, Int)]
        extractDescriptionPlain = \case
            Total ex s _ -> map (,0) ex ++ concatMap (map (,2)) s
            Partial ex s _ -> map (,0) ex ++ concatMap (map (,2)) s
            _ -> []

        extractDescriptionRandom :: CandidateValidDesc -> [(InternalExample, Int)]
        extractDescriptionRandom = \case
            Total _ _ xs -> (take 10 . (unsafePerformIO . shuffleIO) . map (,0)) xs
            Partial _ _ xs -> (take 10 . (unsafePerformIO . shuffleIO) . map (,0)) xs
            _ -> []


        packNodes :: InternalExample -> DataAnalysis
        packNodes (InternalExample dts) = Instance "Root" "!" "" dts (1 + maximum (map height dts))

        unpackNodes :: DataAnalysis -> InternalExample
        unpackNodes (Instance _ _ _ dts _) = InternalExample dts

extractSolution :: Environment -> TypeSkeleton -> UProgram -> ([String], String, String, [(Id, SchemaSkeleton)])
extractSolution env goalType prog = (modules, funcSig, body, argList)
    where
        argList = _arguments env
        modules = "Prelude" : Set.toList (_included_modules env)
        argNames = map fst argList
        argTypes = map snd argList
        monoGoals = map toMonotype argTypes
        funcSig = mkFunctionSigStr (monoGoals ++ [goalType])
        body = mkLambdaStr argNames prog

updateEnvWithBoundTyVars :: SchemaSkeleton -> Environment -> (Environment, TypeSkeleton)
updateEnvWithBoundTyVars (Monotype ty) env = (env, ty)
updateEnvWithBoundTyVars (ForallT x ty) env = updateEnvWithBoundTyVars ty (addTypeVar x env)

updateEnvWithSpecArgs :: TypeSkeleton -> Environment -> (Environment, TypeSkeleton)
updateEnvWithSpecArgs (FunctionT x tArg tRes) env = (addVariable x tArg $ addArgument x tArg env', ret)
    where
        (env', ret) = updateEnvWithSpecArgs tRes env
updateEnvWithSpecArgs ty env = (env, ty)

preprocessEnvFromGoal :: Goal -> (Environment, TypeSkeleton)
preprocessEnvFromGoal goal = updateEnvWithSpecArgs monospec env''
    where
        env''' = gEnvironment goal
        (env'', monospec) = updateEnvWithBoundTyVars (gSpec goal) env'''

replaceId a b = Text.unpack . Text.replace (Text.pack a) (Text.pack b) . Text.pack

matchNiceFunctions :: String -> StateT [(String, String)] IO String
matchNiceFunctions prog | null prog = return prog
matchNiceFunctions prog | head prog == '[' && last prog == ']' =  do
    st <- get
    case lookup prog st of
        Just p -> return p
        Nothing -> do
            let progElmts = dropEnd 1 $ drop 1 prog
            let sepElmts = splitOn "," progElmts
            convertedElmts <- mapM matchNiceFunctions sepElmts
            let newProg = printf "[%s]" (intercalate "," convertedElmts)
            modify ((prog, newProg):)
            return newProg
matchNiceFunctions prog | '\\' `elem` prog && "->" `isInfixOf` prog = do
    st <- get
    case lookup prog st of
        Just p -> return p
        Nothing -> do
            let inputs = [-1, 0, 1, 2]
            let concatInputs = [[], [0], [0,0],[1]]
            let concatOutput = [[], [0,0], [0,0,0,0], [1,1]]
            let prog' = if "..." `isInfixOf` prog then replaceId "..." "" prog else prog
            let stmt = printf "GHC.List.map (%s) %s" prog' (show inputs)
            result <- runStmt stmt
            newProg <- case result of
                "[-3,0,3,6]" -> return "\\x -> x * 3"
                "[0,1,2,3]" -> return "\\x -> x + 1"
                "[1,0,1,4]" -> return "\\x -> x * x"
                _ -> do
                    result2 <- runStmt $ printf "GHC.List.map (%s) %s" prog' (show concatInputs)
                    if result2 == show concatOutput
                        then return "\\x -> x ++ x"
                        else do
                            result3 <- runStmt $ printf "(GHC.List.all ((==) (GHC.List.head %s)) %s, GHC.List.head %s)" result result result
                            if take 5 result3 == "(True"
                                then return $ printf "const %s" (dropEnd 1 $ drop 6 result3)
                                else return prog
            modify ((prog, newProg):)
            return newProg
    where
        runStmt p = do
            let mdls = ["Data.Maybe", "GHC.List", "Data.List", "Data.Eq", "GHC.Char", "Data.Function"]
            result <- LHI.runInterpreter $ do
                LHI.setImports mdls
                -- allow extensions for function execution
                extensions <- LHI.get LHI.languageExtensions
                LHI.set [LHI.languageExtensions LHI.:= (LHI.ExtendedDefaultRules : LHI.ScopedTypeVariables : extensions)]
                LHI.eval p
            return $ either show id result
matchNiceFunctions prog = return prog

niceInputs :: Example -> IO Example
-- niceInputs (Example ins out) = do
--     ins' <- evalStateT (mapM matchNiceFunctions ins) []
--     return (Example ins' out)
-- todo: fix me later; we don't have any nice functions now
niceInputs = return

-- >>> splitConsecutive [1..6]
-- ([1,3,5],[2,4,6])
splitConsecutive :: [a] -> ([a],[a])
splitConsecutive = foldr (\x ~(y2,y1) -> (x:y1, y2)) ([],[])
