{-# LANGUAGE DoAndIfThenElse #-}

module TestCompiler where

-- Failing tests can specify the kind of error that should be thrown with a
-- @shouldFailWith declaration. For example:
--
--   "-- @shouldFailWith TypesDoNotUnify"
--
-- will cause the test to fail unless that module fails to compile with exactly
-- one TypesDoNotUnify error.
--
-- If a module is expected to produce multiple type errors, then use multiple
-- @shouldFailWith lines; for example:
--
--   -- @shouldFailWith TypesDoNotUnify
--   -- @shouldFailWith TypesDoNotUnify
--   -- @shouldFailWith TransitiveExportError
--
-- Warning and failing tests also check their output against the relative
-- golden files (`.out`). The golden files are generated automatically when
-- missing, and can be updated by setting the "HSPEC_ACCEPT" environment
-- variable, e.g. by running `HSPEC_ACCEPT=true stack test`.

import Prelude

import Language.PureScript qualified as P
import Language.PureScript.Interactive.IO (readNodeProcessWithExitCode)

import Control.Arrow ((>>>))
import Control.Monad.State (StateT, evalStateT, gets, modify)
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.Function (on)
import Data.List (sort, stripPrefix, minimumBy)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T


import Control.Monad (forM_, when)

import System.Exit (ExitCode(..))
import System.FilePath (pathSeparator, replaceExtension, takeFileName, (</>))
import System.IO (Handle, hPutStr, hPutStrLn)
import System.IO.UTF8 (readUTF8File)

import Text.Regex.Base (RegexContext(..), RegexMaker(..))
import Text.Regex.TDFA (Regex)

import TestUtils (ExpectedModuleName(..), SupportModules, compile, createOutputFile, getTestFiles, goldenVsString, modulesDir, trim)
import Test.Hspec (Expectation, SpecWith, beforeAllWith, describe, expectationFailure, it, runIO, shouldBe)

spec :: SpecWith SupportModules
spec = do
  bindingVisibilityTests
  passingTests
  warningTests
  failingTests
  optimizeTests

bindingVisibilityTests :: SpecWith SupportModules
bindingVisibilityTests = describe "Binding visibility" $ do
  it "promotes supplied undefined names without changing types, kinds or defined names" $ \_ -> do
    run (P.makeBindingGroupVisible >> snapshot) `shouldBe` Right visible
    run (P.makeBindingGroupVisible >> P.makeBindingGroupVisible >> snapshot) `shouldBe` Right visible

  it "honors both directions of shadowing and restores the whole names scope" $ \_ -> do
    let shadows = M.fromList
          [ (a, (P.tyString, P.Public, P.Defined))
          , (b, (P.tyInt, P.Private, P.Undefined))
          , (c, (P.tyBoolean, P.Private, P.Undefined))
          ]
        promoted = M.fromList
          [ (a, (P.tyString, P.Public, P.Defined))
          , (b, (P.tyInt, P.Private, P.Defined))
          , (c, (P.tyBoolean, P.Private, P.Defined))
          ]
        action = do
          inside <- P.bindNames shadows $ do
            before <- snapshot
            P.makeBindingGroupVisible
            after <- snapshot
            pure [before, after]
          restored <- snapshot
          P.makeBindingGroupVisible
          after <- snapshot
          pure (inside ++ [restored, after])
    run action `shouldBe` Right [shadows, promoted, initial, visible]

  it "restores each nested visible scope, including bindings introduced inside it" $ \_ -> do
    let local = M.singleton c (P.tyBoolean, P.Private, P.Undefined)
        localVisible = M.singleton c (P.tyBoolean, P.Private, P.Defined)
        action = do
          inside <- P.withBindingGroupVisible $ do
            outer <- snapshot
            nested <- P.bindNames local $ do
              inner <- P.withBindingGroupVisible snapshot
              restored <- snapshot
              pure [inner, restored]
            restored <- snapshot
            modify $ \st -> st { P.checkNextType = 17 }
            pure (outer : nested ++ [restored])
          restored <- snapshot
          P.makeBindingGroupVisible
          after <- snapshot
          nextType <- gets P.checkNextType
          pure (inside ++ [restored, after], nextType)
    run action `shouldBe` Right
      ([visible, localVisible `M.union` visible, local `M.union` visible, visible, initial, visible], 17)

  it "observes arbitrary environment replacements and preserves non-name state changes" $ \_ -> do
    let replacement = M.singleton c (P.tyBoolean, P.Public, P.Undefined)
        replacementVisible = M.singleton c (P.tyBoolean, P.Public, P.Defined)
        changed = M.singleton b (P.tyInt, P.Private, P.Defined)
        action = do
          inside <- P.preservingNames $ do
            P.putEnv $ P.initEnvironment { P.names = replacement, P.types = M.empty }
            P.makeBindingGroupVisible
            afterPut <- snapshot
            P.modifyEnv $ \env -> env { P.names = M.singleton b (P.tyInt, P.Private, P.Undefined) }
            P.makeBindingGroupVisible
            afterModify <- snapshot
            pure [afterPut, afterModify]
          restored <- snapshot
          noTypes <- gets (M.null . P.types . P.checkEnv)
          P.makeBindingGroupVisible
          after <- snapshot
          pure (inside ++ [restored, after], noTypes)
    run action `shouldBe` Right ([replacementVisible, changed, initial, visible], True)

  it "rejects cycles again after leaving a function-visible scope" $ \_ -> do
    run (P.checkVisibility a) `shouldBe` Left ["CycleInDeclaration"]
    run (P.checkVisibility b) `shouldBe` Right ()
    run (P.withBindingGroupVisible (P.checkVisibility a)) `shouldBe` Right ()
    run (P.withBindingGroupVisible (P.checkVisibility a) >> P.checkVisibility a)
      `shouldBe` Left ["CycleInDeclaration"]
    run (P.checkVisibility c) `shouldBe` Left ["NameIsUndefined"]
  where
  -- The same identifier under different qualifications must not be conflated.
  a = P.Qualified (P.ByModuleName (P.ModuleName "Outer")) (P.Ident "value")
  b = P.Qualified (P.ByModuleName (P.ModuleName "Imported")) (P.Ident "value")
  c = P.Qualified (P.BySourcePos (P.SourcePos 4 2)) (P.Ident "local")
  initial = M.fromList
    [ (a, (P.tyInt, P.Private, P.Undefined))
    , (b, (P.tyString, P.External, P.Defined))
    ]
  visible = M.fromList
    [ (a, (P.tyInt, P.Private, P.Defined))
    , (b, (P.tyString, P.External, P.Defined))
    ]
  snapshot = gets (P.names . P.checkEnv)
  run :: StateT P.CheckState (Either P.MultipleErrors) a -> Either [T.Text] a
  run action = first (map P.errorCode . P.runMultipleErrors) $
    evalStateT action (P.emptyCheckState (P.initEnvironment { P.names = initial }))

passingTests :: SpecWith SupportModules
passingTests = do
  passingTestCases <- runIO $ getTestFiles "passing"

  describe "Passing examples" $
    beforeAllWith ((<$> createOutputFile logfile) . (,)) $
      forM_ passingTestCases $ \testPurs ->
        it ("'" <> takeFileName (getTestMain testPurs) <> "' should compile and run without error") $ \(support, outputFile) ->
          assertCompiles support testPurs outputFile

warningTests :: SpecWith SupportModules
warningTests = do
  warningTestCases <- runIO $ getTestFiles "warning"

  describe "Warning examples" $
    forM_ warningTestCases $ \testPurs -> do
      let mainPath = getTestMain testPurs
      it ("'" <> takeFileName mainPath <> "' should compile with expected warning(s)") $ \support -> do
        expectedWarnings <- getShouldWarnWith mainPath
        assertCompilesWithWarnings support testPurs expectedWarnings

failingTests :: SpecWith SupportModules
failingTests = do
  failingTestCases <- runIO $ getTestFiles "failing"

  describe "Failing examples" $ do
    forM_ failingTestCases $ \testPurs -> do
      let mainPath = getTestMain testPurs
      it ("'" <> takeFileName mainPath <> "' should fail to compile") $ \support -> do
        expectedFailures <- getShouldFailWith mainPath
        assertDoesNotCompile support testPurs expectedFailures

optimizeTests :: SpecWith SupportModules
optimizeTests = do
  optimizeTestCases <- runIO $ getTestFiles "optimize"

  describe "Optimization examples" $
    forM_ optimizeTestCases $ \testPurs ->
      it ("'" <> takeFileName (getTestMain testPurs) <> "' should compile to expected output") $ \support ->
        assertCompilesToExpectedOutput support testPurs

checkShouldReport :: [String] -> (P.MultipleErrors -> String) -> P.MultipleErrors -> Expectation
checkShouldReport expected prettyPrintDiagnostics errs =
  let actual = map P.errorCode $ P.runMultipleErrors errs
  in if sort expected == sort (map T.unpack actual)
    then checkPositioned errs
    else expectationFailure $ "Expected these diagnostics: " ++ show expected ++ ", but got these: "
      ++ show actual ++ ", full diagnostic messages: \n"
      ++ prettyPrintDiagnostics errs

checkPositioned :: P.MultipleErrors -> Expectation
checkPositioned errs =
  case mapMaybe guardSpans (P.runMultipleErrors errs) of
    [] ->
      pure ()
    errs' ->
      expectationFailure
        $ "Found diagnostics with missing source spans:\n"
        ++ unlines (map (P.renderBox . P.prettyPrintSingleError P.defaultPPEOptions) errs')
  where
  guardSpans :: P.ErrorMessage -> Maybe P.ErrorMessage
  guardSpans err = case P.errorSpan err of
    Just ss | not $ all isNonsenseSpan ss -> Nothing
    _ -> Just err

  isNonsenseSpan :: P.SourceSpan -> Bool
  isNonsenseSpan (P.SourceSpan spanName spanStart spanEnd) =
    spanName == "" || spanName == "<module>" || (spanStart == emptyPos && spanEnd == emptyPos)

  emptyPos :: P.SourcePos
  emptyPos = P.SourcePos 0 0

assertCompiles
  :: SupportModules
  -> [FilePath]
  -> Handle
  -> Expectation
assertCompiles support inputFiles outputFile = do
  (fileContents, (result, _)) <- compile (Just IsMain) support inputFiles
  let errorOptions = P.defaultPPEOptions { P.ppeFileContents = fileContents }
  case result of
    Left errs -> expectationFailure . P.prettyPrintMultipleErrors errorOptions $ errs
    Right _ -> do
      let entryPoint = modulesDir </> "index.js"
      writeFile entryPoint "import('./Main/index.js').then(({ main }) => main());"
      nodeResult <- readNodeProcessWithExitCode Nothing [entryPoint] ""
      hPutStrLn outputFile $ "\n" <> takeFileName (last inputFiles) <> ":"
      case nodeResult of
        Right (ExitSuccess, out, err)
          | not (null err) -> expectationFailure $ "Test wrote to stderr:\n\n" <> err
          | not (null out) && trim (last (lines out)) == "Done" -> hPutStr outputFile out
          | otherwise -> expectationFailure $ "Test did not finish with 'Done':\n\n" <> out
        Right (ExitFailure _, _, err) -> expectationFailure err
        Left err -> expectationFailure err

assertCompilesWithWarnings
  :: SupportModules
  -> [FilePath]
  -> [String]
  -> Expectation
assertCompilesWithWarnings support inputFiles shouldWarnWith = do
  (fileContents, result'@(result, warnings)) <- compile Nothing support inputFiles
  let errorOptions = P.defaultPPEOptions { P.ppeFileContents = fileContents }
  case result of
    Left errs ->
      expectationFailure . P.prettyPrintMultipleErrors errorOptions $ errs
    Right _ -> do
      checkShouldReport shouldWarnWith (P.prettyPrintMultipleWarnings errorOptions) warnings
      goldenVsString
        (replaceExtension (getTestMain inputFiles) ".out")
        (return . T.encodeUtf8 . T.pack $ printDiagnosticsForGoldenTest fileContents result')

assertDoesNotCompile
  :: SupportModules
  -> [FilePath]
  -> [String]
  -> Expectation
assertDoesNotCompile support inputFiles shouldFailWith = do
  (fileContents, result) <- compile Nothing support inputFiles
  let errorOptions = P.defaultPPEOptions { P.ppeFileContents = fileContents }
  case fst result of
    Left errs -> do
      when (null shouldFailWith)
        (expectationFailure $
          "shouldFailWith declaration is missing (errors were: "
          ++ show (map P.errorCode (P.runMultipleErrors errs))
          ++ ")")
      checkShouldReport shouldFailWith (P.prettyPrintMultipleErrors errorOptions) errs
      goldenVsString
        (replaceExtension (getTestMain inputFiles) ".out")
        (return . T.encodeUtf8 . T.pack $ printDiagnosticsForGoldenTest fileContents result)
    Right _ ->
      expectationFailure "Should not have compiled"

assertCompilesToExpectedOutput
  :: SupportModules
  -> [FilePath]
  -> Expectation
assertCompilesToExpectedOutput support inputFiles = do
  (fileContents, (result, _)) <- compile Nothing support inputFiles
  let errorOptions = P.defaultPPEOptions { P.ppeFileContents = fileContents }
  case result of
    Left errs -> expectationFailure . P.prettyPrintMultipleErrors errorOptions $ errs
    Right _ ->
      goldenVsString
        (replaceExtension (getTestMain inputFiles) ".out.js")
        (BS.readFile $ modulesDir </> "Main/index.js")

-- Prints a set of diagnostics (i.e. errors or warnings) as a string, in order
-- to compare it to the contents of a golden test file.
printDiagnosticsForGoldenTest :: [(FilePath, T.Text)] -> (Either P.MultipleErrors a, P.MultipleErrors) -> String
printDiagnosticsForGoldenTest fileContents (result, warnings) =
  normalizePaths $ case result of
    Left errs ->
      -- TODO: should probably include warnings when failing?
      P.prettyPrintMultipleErrors errorOptions errs
    Right _ ->
      P.prettyPrintMultipleWarnings errorOptions warnings
  where
  errorOptions = P.defaultPPEOptions { P.ppeFileContents = fileContents }

-- Replaces Windows-style paths in an error or warning with POSIX paths
normalizePaths :: String -> String
normalizePaths = if pathSeparator == '\\'
  then replaceMatches " [0-9A-Za-z_-]+(\\\\[0-9A-Za-z_-]+)+\\.[A-Za-z]+\\>" (map turnSlash)
  else id
  where
    turnSlash '\\' = '/'
    turnSlash c = c

-- Uses a function to replace all matches of a regular expression in a string
replaceMatches :: String -> (String -> String) -> String -> String
replaceMatches reString phi = go
  where
    re :: Regex
    re = makeRegex reString
    go :: String -> String
    go haystack =
      let (prefix, needle, suffix) = match re haystack
      in prefix ++ (if null needle then "" else phi needle ++ go suffix)

-- Takes the test entry point from a group of purs files - this is determined
-- by the file with the shortest path name, as everything but the main file
-- will be under a subdirectory.
getTestMain :: [FilePath] -> FilePath
getTestMain = minimumBy (compare `on` length)

-- Scans a file for @shouldFailWith directives in the comments, used to
-- determine expected failures
getShouldFailWith :: FilePath -> IO [String]
getShouldFailWith = extractPragma "shouldFailWith"

-- Scans a file for @shouldWarnWith directives in the comments, used to
-- determine expected warnings
getShouldWarnWith :: FilePath -> IO [String]
getShouldWarnWith = extractPragma "shouldWarnWith"

extractPragma :: String -> FilePath -> IO [String]
extractPragma pragma = fmap go . readUTF8File
  where
    go = lines >>> mapMaybe (stripPrefix ("-- @" ++ pragma ++ " ")) >>> map trim


logfile :: FilePath
logfile = "psc-tests.out"
