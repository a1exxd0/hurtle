-- This is the main entry point for your Hurtle viewer.
import Hurtle.Types
import Hurtle.FileReader
import Hurtle.CodeGeneration
import Data.Map.Strict as Map
import Control.Monad.State.Strict
import Text.Megaparsec
import Hurtle.Tokenizer
--import System.IO(readFile)

main :: IO ()
main = do
    putStrLn "Enter file name: "
    fileName <- getLine
    input <- readFileToLower fileName
    let initialState = []
    case runParser (runStateT (runTokenParser parseTokens) initialState) "" (input ++ "\n") of
        Left err -> putStrLn $ "Tokenization error: " ++ errorBundlePretty err
        Right (result, tokenState) -> do
            -- putStrLn $ "Parsed result: " ++ show result
            putStrLn $ "Token state: " ++ Hurtle.Types.show tokenState
            let initialState2 = HogoProgram { varTable = Map.empty, procTable = Map.empty, code = [] }
            case runParser (runStateT (runHogoParser parseHogo) initialState2) "" tokenState of
                Left err2 -> putStrLn $ "Syntax error: " ++ formatError err2
                Right (res, final) -> do
                    putStrLn $ "Final state: " ++ show final

