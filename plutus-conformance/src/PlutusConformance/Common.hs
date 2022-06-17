{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Plutus conformance test suite library. -}
module PlutusConformance.Common where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import PlutusCore.Core (defaultVersion)
import PlutusCore.Default (DefaultFun, DefaultUni)
import PlutusCore.Error (ParserErrorBundle)
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults (defaultCekParameters)
import PlutusCore.Name (Name)
import PlutusCore.Quote (runQuoteT)
import PlutusPrelude
import System.Directory
import System.FilePath (takeBaseName, (</>))
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Providers
import Text.Megaparsec (SourcePos)
import UntypedPlutusCore.Core.Type qualified as UPLC
import UntypedPlutusCore.Evaluation.Machine.Cek (evaluateCekNoEmit)
import UntypedPlutusCore.Parser qualified as UPLC
import Witherable

type UplcProg = UPLC.Program Name DefaultUni DefaultFun ()

type UplcEvaluator = UplcProg -> Maybe UplcProg

data UplcEvaluationTest =
    MkUplcEvaluationTest {
       runner    :: UplcEvaluator
       , testDir :: FilePath
    }

instance IsTest UplcEvaluationTest where
    run _ MkUplcEvaluationTest{testDir,runner} _ = do
        let name = takeBaseName testDir
        input <- T.readFile $ testDir </> name <> ".uplc"
        let
            parsed :: Either ParserErrorBundle (UPLC.Program Name DefaultUni DefaultFun SourcePos)
            parsed = runQuoteT $ UPLC.parseProgram input

        expected <- T.readFile $ testDir </> name <> ".uplc.expected"
        let checkContents c | c == expected = pure (testPassed "")
            checkContents c = pure (testFailed (show c))

        case parsed of
            Left _ -> checkContents shownParseError
            Right p -> do
               case runner (void p) of
                   Nothing -> checkContents shownEvaluationFailure
                   Just p' -> checkContents (display p')

    testOptions = pure []

-- Common functions for all tests

{- | The default shown text when a parse error occurs.
We don't want to show the detailed parse errors so that
users of the test suite can produce this expected outputs more easily. -}
shownParseError :: T.Text
shownParseError = "parse error"

-- | The default shown text when evaluation fails.
shownEvaluationFailure :: T.Text
shownEvaluationFailure = "evaluation failure"

-- For UPLC evaluation tests

termToProg :: UPLC.Term Name DefaultUni DefaultFun () -> UplcProg
termToProg = UPLC.Program () (defaultVersion ())

-- | Our `runner` for the UPLC tests is the CEK machine.
evalUplcProg :: UplcEvaluator
evalUplcProg p =
    let eitherExceptionProg =
            fmap
                termToProg
                (evaluateCekNoEmit defaultCekParameters (UPLC._progTerm p))
    in
        case eitherExceptionProg of
            Left _     -> Nothing
            Right prog -> Just prog

-- | Walk a file tree, making test groups for directories with subdirectores,
-- and test cases for directories without.
discoverTests :: IsTest t => (FilePath -> t) -> FilePath -> IO TestTree
discoverTests tester dir = do
    let name = takeBaseName dir
    children <- listDirectory dir
    tts <- (flip wither) children $ \child -> do
        let fullPath = dir </> child
        isDir <- doesDirectoryExist fullPath
        if isDir then Just <$> discoverTests tester fullPath
        else pure Nothing
    -- no children, this is a test case directory
    if null tts then pure $ singleTest name $ tester dir
    -- has children, so it's a grouping directory
    else pure $ testGroup name tts

-- | Run the tests given a `runner` that evaluates UPLC programs.
runUplcEvalTests ::
    UplcEvaluator -- ^ The action to run the input through for the tests.
    -> IO ()
runUplcEvalTests runner = do
    tests <- discoverTests (\dir -> MkUplcEvaluationTest runner dir) "uplc/evaluation"
    defaultMain $ testGroup "UPLC evaluation tests" [tests]
