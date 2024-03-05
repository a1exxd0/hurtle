-- This is the main entry point for your Hurtle viewer.
import Hurtle.Types
import Hurtle.FileReader
--import Hurtle.Parser

main :: IO ()
main = do
    putStrLn "Enter file name: "
    fn <- getLine
    res <- readFileToLower fn
    putStrLn res