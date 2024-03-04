module Hurtle.Parser where

import Hurtle.Types

-- You'll probably want to refer to https://hackage.haskell.org/package/megaparsec for documentation of the Megaparsec library.
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Char

-- | Helpers

satisfyIgnoreCase :: (String -> Bool) -> Parser String
satisfyIgnoreCase f = takeWhileP (Just "string") (\s -> f (map toLower s))

-- | Variable Parsers

parseVariableByName :: Parser Variable
parseVariableByName = do
    _ <- satisfy (==':')
    name <- takeWhileP (Just "character") (\c -> c /= ' ' && c /= '\n' && c /= ',')
    pure (Variable $ Left $ map toLower name)

parseVariableByValue :: Parser Variable
parseVariableByValue = do
    num <- some digitChar
    _ <- char '.'
    decimal <- some digitChar
    _ <- char ' '
    pure $ Variable $ Right (read (num ++ "." ++ decimal) :: Float)
  <|> do
    num <- some digitChar
    _ <- char ' '
    return $ Variable $ Right (read num :: Float)

parseByNameByValue :: Parser Variable
parseByNameByValue = parseVariableByName <|> parseVariableByValue

parseSum :: Parser Variable
parseSum = do
    _ <- satisfyIgnoreCase (=="sum")
    _ <- skipMany space
    var1 <- parseByNameByValue
    _ <- skipMany space
    var2 <- parseByNameByValue
    _ <- takeWhileP (Just "whitespace") (\c -> c /= ' ' && c /= '\n' && c /= ',')
    pure $ Sum var1 var2


parseHogo :: Parser HogoProgram
parseHogo = error "Implement me :)"