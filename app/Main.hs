-- This is the main entry point for your Hurtle viewer.
import Hurtle.Types
import Hurtle.FileReader
import Hurtle.CodeGeneration
import Data.Map.Strict as Map
import Control.Monad.State.Strict
import Text.Megaparsec
import Hurtle.Tokenizer
import System.Console.ANSI
--import System.IO(readFile)

main :: IO ()
main = do
    setSGR [System.Console.ANSI.SetColor Foreground Vivid Yellow, SetConsoleIntensity BoldIntensity]
    putStrLn "\n\n\nEnter file name (scoped to project directory): "
    fileName <- getLine
    input <- readFileToLower fileName
    runTokenization input

runTokenization :: String -> IO ()
runTokenization input = do
    let initialState = []
    case runParser (runStateT (runTokenParser parseTokens) initialState) "" (input ++ "\n") of
        Left err -> putStrLn $ "Tokenization error: " ++ errorBundlePretty err
        Right (result, tokenState) -> do
            putStrLn $ "Token state: " ++ show tokenState
            runSyntaxAnalysis tokenState

runSyntaxAnalysis :: [TOKENS] -> IO ()
runSyntaxAnalysis tokenState = do
    let initialState2 = HogoProgram { varTable = Map.empty, procTable = Map.empty, code = [] }
    case runParser (runStateT (runHogoParser parseHogo) initialState2) "" tokenState of
        Left err2 -> putStrLn $ "Syntax error: " ++ formatError err2
        Right (res, final) -> do
            putStrLn $ "Final state: " ++ show final