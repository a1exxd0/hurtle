module Hurtle.Parser where

import Hurtle.Types

-- You'll probably want to refer to https://hackage.haskell.org/package/megaparsec for documentation of the Megaparsec library.
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Char

-- | Helpers

parseToNewLine :: Parser ()
parseToNewLine = do 
    hspace
    _ <- satisfy (==',') <|> newline
    pure ()

-- | Variable Parsers

parseVariable :: Parser Variable
parseVariable = undefined

-- | HogoCode Parsers



-- | HogoProgram Parsers


parseHogo :: Parser HogoProgram
parseHogo = error "Implement me :)"