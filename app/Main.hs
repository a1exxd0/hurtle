-- This is the main entry point for your Hurtle viewer.
import Hurtle.Types
import Hurtle.FileReader
import Hurtle.Parser
import Data.Map.Strict as Map
import Control.Monad.State.Strict
import Text.Megaparsec
--import System.IO(readFile)

main :: IO ()
main = do
    putStrLn "Enter file name: "
    fileName <- getLine
    input <- readFileToLower fileName
    let initialState = HogoProgram { varTable = Map.empty, procTable = Map.empty, code = [] }
    case runParser (runStateT (runHogoParser parseHogo) initialState) "" (input ++ "\n") of
        Left err -> putStrLn $ "Parse error: " ++ errorBundlePretty err
        Right (result, finalState) -> do
            -- putStrLn $ "Parsed result: " ++ show result
            putStrLn $ "Final state: " ++ show finalState