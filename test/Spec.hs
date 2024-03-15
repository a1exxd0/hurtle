{-
Tasty is the testing library that is used to specify tests.
The backends "tasty-hunit" and "tasty-quickcheck" specify the way that unit 
tests and property tests (respectively) are written.
-}
{-# OPTIONS_GHC -Wno-all #-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

import Hurtle.Types
import Hurtle.CodeGeneration
import Hurtle.Tokenizer

import System.Console.ANSI (clearScreen)
import Test.Tasty
  ( TestTree, testGroup,
  )
import Test.Tasty.Muffled ( muffledMain )
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.List (findIndex, nub, inits, isInfixOf)
import Data.Function (on)
import Data.Tuple (swap)

import Data.Set (Set)
import qualified Data.Set as Set
import Crypto.Hash.MD5 (hash)
import Data.ByteString.Char8 (pack)

import System.Directory (getDirectoryContents, doesFileExist)
import System.FilePath (takeExtension, dropExtension)

import Text.Megaparsec
import Control.Monad (forM)
import Data.List (isSuffixOf, sort)
import Hurtle.FileReader
import Hurtle.Tokenizer
import Hurtle.ShowHurtle
import Hurtle.CodeGeneration
import Data.Map.Strict as Map
import Control.Monad.State.Strict



{-
  To the marker! I've changed the tests so that they run with my StateT wrapper to make
  it easier for you. Previously, it asssumed HogoParser was a Parser HogoProgram (where
  this was a list of HogoCodes), but I've changed it to be a StateT ___, meaning the 
  running method is different. This won't display any files but it will show you outputs
  and errors for parsing

  I've added Alex's extra tests too, but this does not display in full form the functionality
  of my program. If you haven't already, check out test/spiraloctagons.hogo.

-}

main :: IO ()
main = do

  passingFiles <- Prelude.filter (\fp -> takeExtension fp == ".hogo") 
    <$> getDirectoryContents "examples/passing"
  passingCases <- forM (sort passingFiles) $ \fp -> do
    let fullPath = "examples/passing/" ++ fp
        expectedPath = dropExtension fullPath <> ".expected"
    hasExpected <- doesFileExist expectedPath
    let testName = if hasExpected then fp else fp ++ " (no expected output)"
    let test = testCase testName do
      -- | CHANGED TO ACCOMODATE MTL
          input <- readFileToLower fullPath

          let initialState = []
          case runParser (runStateT (runTokenParser parseTokens) initialState) fp (input ++ "\n") of
              Left err -> do assertFailure $ "Tokenization error: " ++ errorBundlePretty err
              Right (_, tokenState) -> do
                  putStrLn $ "Token state: " ++ show tokenState
                  let initialState2 = HogoProgram { varTable = Map.empty, procTable = Map.empty, code = [] }
                  case runParser (runStateT (runHogoParser parseHogo) initialState2) fp tokenState of
                      Left err2 -> do assertFailure $ "Syntax error: " ++ formatError err2
                      Right (_, final) -> do
                          putStrLn $ "Final state: " ++ show final
                          pure ()
    return test
  
  failingFiles <- Prelude.filter (".hogo" `isSuffixOf`) 
    <$> getDirectoryContents "examples/failing"
  failingCases <- forM (sort failingFiles) $ \fp -> do
    let fullPath = "examples/failing/" ++ fp
    let test = testCase fp do
          input <- readFileToLower fullPath
          let initialState = []
          case runParser (runStateT (runTokenParser parseTokens) initialState) fp (input ++ "\n") of
              Left err -> do assertFailure $ "Tokenization error: " ++ errorBundlePretty err
              Right (_, tokenState) -> do
                  putStrLn $ "Token state: " ++ show tokenState
                  let initialState2 = HogoProgram { varTable = Map.empty, procTable = Map.empty, code = [] }
                  case runParser (runStateT (runHogoParser parseHogo) initialState2) fp tokenState of
                      Left err2 -> do assertFailure $ "Syntax error: " ++ formatError err2
                      Right (_, final) -> do
                          putStrLn $ "Final state: " ++ show final
                          pure ()
    return test

  clearScreen
  muffledMain $
    testGroup
      "Examples"
      [ testGroup "Passing cases (should all parse correctly)" passingCases
      , testGroup "Failing cases (should all fail to parse)" failingCases
      ]
