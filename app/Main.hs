-- This is the main entry point for your Hurtle viewer.
import Hurtle.Types
import Hurtle.FileReader
import Hurtle.CodeGeneration
import Hurtle.ShowHurtle
import Data.Map.Strict as Map
import Control.Monad.State.Strict
import Text.Megaparsec
import Hurtle.Tokenizer
import System.Console.ANSI
import Hatch
--import System.IO(readFile)

main :: IO ()
main = do
    setSGR [System.Console.ANSI.SetColor Foreground Vivid White, SetConsoleIntensity BoldIntensity]
    putStrLn "\n\n\nEnter file name (scoped to project directory): "
    fileName <- getLine
    setSGR [System.Console.ANSI.SetColor Foreground Vivid Yellow, SetConsoleIntensity BoldIntensity]
    input <- readFileToLower "test/test.hogo"
    runTokenization input

runTokenization :: String -> IO ()
runTokenization input = do
    let initialState = []
    case runParser (runStateT (runTokenParser parseTokens) initialState) "" (input ++ "\n") of
        Left err -> do
            setSGR [System.Console.ANSI.SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity]
            putStrLn $ "Tokenization error: " ++ errorBundlePretty err
        Right (_, tokenState) -> do
            putStrLn $ "Token state: " ++ show tokenState
            runSyntaxAnalysis tokenState

runSyntaxAnalysis :: [TOKENS] -> IO ()
runSyntaxAnalysis tokenState = do
    let initialState2 = HogoProgram { varTable = Map.empty, procTable = Map.empty, code = [] }
    case runParser (runStateT (runHogoParser parseHogo) initialState2) "" tokenState of
        Left err2 -> do
            setSGR [System.Console.ANSI.SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity]
            putStrLn $ "Syntax error: " ++ formatError err2
        Right (_, final) -> do
            setSGR [System.Console.ANSI.SetColor Foreground Vivid Green, SetConsoleIntensity BoldIntensity]
            putStrLn $ "Final state: " ++ show final
            runInterpreter final

runInterpreter :: HogoProgram -> IO ()
runInterpreter prog = runAnimation (animation prog)