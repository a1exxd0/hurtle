-- This is the main entry point for your Hurtle viewer.
import Hurtle.Types
import Hurtle.FileReader
import Hurtle.CodeGeneration
import Hurtle.ShowHurtle hiding (initialState)
import Data.Map.Strict as Map
import Control.Monad.State.Strict
import Text.Megaparsec
import Hurtle.Tokenizer
import System.Console.ANSI
import Hatch

setConsoleDetails 
    :: Color                -- ^ Colour you want to set console text
    -> ConsoleIntensity     -- ^ NormalIntensity or BoldIntensity (for text strength)
    -> IO ()
setConsoleDetails col int = 
    setSGR [System.Console.ANSI.SetColor Foreground Vivid col, SetConsoleIntensity int]

main :: IO ()
main = do
    setConsoleDetails White BoldIntensity
    putStrLn "\n\n\nEnter file name (scoped to project directory, i.e. test/<name>.hogo): "
    fileName <- getLine
    setConsoleDetails Yellow BoldIntensity
    input <- readFileToLower fileName
    runTokenization input
    
-- | Run a string through tokenization stage and follow it with
--   the syntax analysis stage if successful, else show error
runTokenization 
    :: String           -- ^ Input string
    -> IO ()
runTokenization input = do
    let initialState = []
    case runParser (runStateT (runTokenParser parseTokens) initialState) "" (input ++ "\n") of
        Left err -> do
            setConsoleDetails Red BoldIntensity
            putStrLn $ "Tokenization error: " ++ errorBundlePretty err
        Right (_, tokenState) -> do
            putStrLn $ "Token state: " ++ show tokenState
            runSyntaxAnalysis tokenState

-- | Run a list of TOKENS through syntax analysis and run the 
--   interpreter if successful following, else print error.
runSyntaxAnalysis 
    :: [TOKENS]                 -- ^ Input token list
    -> IO ()
runSyntaxAnalysis tokenState = do
    let initialState2 = HogoProgram { varTable = Map.empty, procTable = Map.empty, code = [] }
    case runParser (runStateT (runHogoParser parseHogo) initialState2) "" tokenState of
        Left err2 -> do
            setConsoleDetails Red BoldIntensity
            putStrLn $ "Syntax error: " ++ formatError err2
        Right (_, final) -> do
            setConsoleDetails Green BoldIntensity
            putStrLn $ "Final state: " ++ show final
            setConsoleDetails White NormalIntensity
            putStrLn "" -- to reset console colour

            runInterpreter final

-- | Call the Hatch runAnimation function to display frame states
--   of a HogoProgram
runInterpreter 
    :: HogoProgram          -- ^ HogoProgram input
    -> IO ()
runInterpreter prog = runAnimation (animation prog)