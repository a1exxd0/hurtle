module Hurtle.Parser where

import Hurtle.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Char
import Data.Map.Strict as Map
import Control.Monad.State.Strict
import Data.Maybe

-- | Helpers

liftHogo :: Parser a -> HogoParser a
liftHogo p = HogoParser $ lift p

parseToEOF :: Parser ()
parseToEOF = do
    hspace
    _ <- try eof
    pure ()

parseToNL :: Parser ()
parseToNL = do 
    hspace
    _ <- satisfy (\c -> c ==',' || c == '\n')
    pure ()

parseToNewLine :: Parser ()
parseToNewLine = parseToEOF <|> parseToNL

float :: Parser Float
float = do
    integerPart <- some digitChar
    decimalPart <- optional $ char '.' *> some digitChar
    let value = case decimalPart of
            Nothing -> read integerPart
            Just dPart -> read (integerPart ++ "." ++ dPart)
    space
    pure value

-- | Update code

updateCode :: HogoCode -> HogoParser ()
updateCode c = do
    curr <- get
    let updated = curr {code = code curr ++ pure c}
    put updated

-- | Update variable

updateVariable :: String -> Variable -> HogoParser ()
updateVariable name (Variable kv) = do
    curr <- get
    let updated = curr {varTable = Map.insert name kv (varTable curr)}
    put updated

-- | Variable Declaration Parsers

parseVariableDeclaration :: HogoParser ()
parseVariableDeclaration = do
    _ <- liftHogo $ chunk "make"
    liftHogo hspace
    _ <- liftHogo $ satisfy (=='"')
    varName <- liftHogo $ manyTill anySingle ( void (
                satisfy (\c -> c == ' ' || c == ',' || c == '\n')
            ))
    liftHogo hspace
    val <- parseVariable
    updateVariable varName val


-- | Variable Parsers

parseVariableByValue :: Parser Variable
parseVariableByValue = do 
    space >> Variable . Value <$> float

parseVariableByName :: Parser Variable
parseVariableByName = do
    space
    _ <- satisfy (==':')
    varName <- manyTill anySingle ( void (
                satisfy (\c -> c == ' ' || c == ',' || c == '\n')
            ))
    pure (Variable $ Key varName)

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
    _ <- liftHogo parseToNewLine
    updateCode Home

parseSingleArg :: HogoParser ()
parseSingleArg = do
    op <- liftHogo ( 
        parseForward <|> parseBackward <|> parseLeft <|>
        parseRight <|> parseSetWidth <|> parseSetColor )
    _ <- liftHogo space
    var <- parseVariable
    _ <- liftHogo parseToNewLine
    updateCode $ op var
    where
        parseForward    = chunk "forward"   >> pure Forward
        parseBackward   = chunk "back"      >> pure Backward
        parseLeft       = chunk "left"      >> pure GoLeft
        parseRight      = chunk "right"     >> pure GoRight
        parseSetWidth   = chunk "setwidth"  >> pure SetWidth
        parseSetColor   = chunk "setcolor"  >> pure SetColor

parseHogoCode :: HogoParser () 
parseHogoCode = do
    parseHome <|> parseSingleArg <|> parseVariableDeclaration <|> liftHogo parseToNewLine
    end <- liftHogo atEnd
    unless end parseHogoCode

-- | HogoProgram Parsers

parseHogo :: HogoParser HogoProgram
parseHogo = do
    parseHogoCode
    get