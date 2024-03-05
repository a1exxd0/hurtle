module Hurtle.Parser where

import Hurtle.Types

-- You'll probably want to refer to https://hackage.haskell.org/package/megaparsec for documentation of the Megaparsec library.
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Char
import Data.Map.Strict
import Control.Monad.State.Strict
import Data.Maybe
import Debug.Trace

-- | Helpers

liftHogo :: Parser a -> HogoParser a
liftHogo p = HogoParser $ lift p

parseToEOF :: Parser ()
parseToEOF = do
    _ <- try eof
    pure ()

parseToNL :: Parser ()
parseToNL = do 
    _ <- space
    _ <- try eof >> satisfy (\c -> c ==',' || c == '\n')
    pure ()

parseToNewLine :: HogoParser ()
parseToNewLine = liftHogo (parseToEOF <|> parseToNL)

-- | Comment skipping
skipToNewLine :: HogoParser ()
skipToNewLine = do
    _ <- liftHogo (takeWhileP Nothing $ \c -> c /= '\n')
    _ <- liftHogo anySingle
    pure ()

float :: Parser Float
float = do
    integerPart <- some digitChar
    decimalPart <- optional $ char '.' *> some digitChar
    let value = case decimalPart of
            Nothing -> read integerPart
            Just dPart -> read (integerPart ++ "." ++ dPart)
    _ <- space
    pure value

-- | Update code

updateCode :: HogoCode -> HogoParser ()
updateCode c = do
    curr <- get
    let updated = curr {code = code curr ++ pure c}
    put updated

-- | Variable Declaration Parsers

parseVariableDeclaration :: HogoParser ()
parseVariableDeclaration = undefined

-- | Variable Parsers

parseVariableByValue :: Parser Variable
parseVariableByValue = do Variable . Right <$> float

parseVariableByName :: Parser Variable
parseVariableByName = do
    _ <- satisfy (==':')
    varName <- manyTill anySingle ( void (
                satisfy (\c -> c == ' ' || c == ',' || c == '\n')
            ))
    _ <- hspace
    pure (Variable $ Left varName)

parseVariable :: HogoParser Variable
parseVariable = liftHogo (parseVariableByName <|> parseVariableByValue)

-- | Function Declaration Parsers

parseFunctionDeclaration :: HogoParser ()
parseFunctionDeclaration = undefined

-- | Function Call Parsers

parseFunctionCall :: HogoParser String
parseFunctionCall = undefined

-- | HogoCode Parsers

parseHome :: HogoParser ()
parseHome = do
    _ <- liftHogo $ chunk "home"
    _ <- parseToNewLine
    updateCode Home

parseSingleArg :: HogoParser ()
parseSingleArg = do
    op <- liftHogo ( parseForward <|> parseBackward <|> parseLeft <|>
        parseRight <|> parseSetWidth <|> parseSetColor )
    _ <- liftHogo space
    var <- parseVariable
    _ <- parseToNewLine
    updateCode $ op var
    where
        parseForward = chunk "forward" >> pure Forward
        parseBackward = chunk "back" >> pure Backward
        parseLeft = chunk "left" >> pure GoLeft
        parseRight = chunk "right" >> pure GoRight
        parseSetWidth = chunk "setwidth" >> pure SetWidth
        parseSetColor = chunk "setcolor" >> pure SetColor

parseHogoCode :: HogoParser () 
parseHogoCode = do 
    parseHome <|> parseSingleArg <|> parseToNewLine
    continueParsing where
        continueParsing = do
            end <- liftHogo $ optional eof
            unless (isJust end) $ do
                parseHogoCode

-- | HogoProgram Parsers

parseHogo :: HogoParser HogoProgram
parseHogo = do
    parseHogoCode
    get