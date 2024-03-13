module Hurtle.Tokenizer where

import Hurtle.Types
    ( TokenParser(TokenParser),
      Parser,
      TOKENS(VALUE, FOR, REPEAT, RIGHTBRACKET, LEFTBRACKET, NEWLINE, SUM,
             DIFFERENCE, MULTIPLY, MAKE, SPEECHMARK, COLON, TO, END, BACK,
             FORWARD, HOME, SETWIDTH, SETCOLOR, PENUP, PENDOWN, CLS, NAME) )

import Text.Megaparsec
    ( optional,
      atEnd,
      chunk,
      satisfy,
      manyTill,
      some,
      (<|>),
      MonadParsec(lookAhead, try, eof) )
      
import Text.Megaparsec.Char
    ( alphaNumChar, char, digitChar, hspace, hspace1, newline )
import Data.Char ()
import Data.Map.Strict as Map ()
import Control.Monad.State.Strict
    ( unless, void, MonadState(get, put), MonadTrans(lift) )
import Data.Maybe ()
import Text.Megaparsec.Char.Lexer ( skipLineComment )

-- tokens put in reverse
-- | Helpers

liftToken :: Parser a -> TokenParser a
liftToken p = TokenParser $ lift p

parseToEOFWithSpace :: Parser ()
parseToEOFWithSpace = do
    hspace
    _ <- try eof
    pure ()

parseToEOFNoSpace :: Parser ()
parseToEOFNoSpace = do
    _ <- try eof
    pure ()

parseWithSpace :: Parser a -> Parser ()
parseWithSpace ps = do
    hspace
    _ <- ps
    hspace
    pure ()

parseCharWithSpace :: Char -> Parser ()
parseCharWithSpace c = do
    parseWithSpace $ satisfy (==c)

parseWordWithSpace :: String -> Parser ()
parseWordWithSpace word = do
    parseWithSpace $ chunk word

parseWordForceSpace :: String -> Parser ()
parseWordForceSpace word = do
    hspace
    _ <- chunk word
    hspace1 <|> try eof

parseComment :: Parser ()
parseComment = do
    hspace
    _ <- skipLineComment ";" -- stops before newline
    pure ()

float :: Parser Float
float = do
    hspace
    integerPart <- some digitChar
    decimalPart <- optional $ char '.' *> some digitChar
    let value = case decimalPart of
            Nothing -> read integerPart
            Just dPart -> read (integerPart ++ "." ++ dPart)
    pure value

-- | Control flow

parseFor :: Parser TOKENS
parseFor = do
    _ <- parseWordWithSpace "for"
    pure FOR

parseRepeat :: Parser TOKENS
parseRepeat = do
    _ <- parseWordWithSpace "repeat"
    pure REPEAT

parseControlFlow :: TokenParser ()
parseControlFlow = do
    x <- liftToken $ parseFor <|> parseRepeat
    curr <- get
    put (x:curr)

-- | Brackets

parseRightBracket :: Parser TOKENS
parseRightBracket = do
    _ <- parseCharWithSpace ']'
    pure RIGHTBRACKET
    
parseLeftBracket :: Parser TOKENS
parseLeftBracket = do
    _ <- parseCharWithSpace '['
    pure LEFTBRACKET

parseBracket :: TokenParser ()
parseBracket = do
    x <- liftToken $ parseLeftBracket <|> parseRightBracket
    curr <- get
    put $ x:curr

-- | Markers

parseCommaOrNewLine :: TokenParser ()
parseCommaOrNewLine = do
    _ <- liftToken $ parseWithSpace (satisfy (==',') <|> newline)
    curr <- get

    if head curr == NEWLINE 
    then put curr
    else put $ NEWLINE:curr

-- | Maths

parseSum :: Parser TOKENS
parseSum = do
    _ <- parseWordForceSpace "sum"
    pure SUM

parseDifference :: Parser TOKENS
parseDifference = do
    _ <- parseWordForceSpace "difference"
    pure DIFFERENCE

parseMultiply :: Parser TOKENS
parseMultiply = do
    _ <- parseWordForceSpace "multiply"
    pure MULTIPLY

parseMaths :: TokenParser ()
parseMaths = do
    x <- liftToken $ parseSum <|> parseDifference <|> parseMultiply
    curr <- get
    put $ x:curr

-- | Variable creation & use

parseMake :: Parser TOKENS
parseMake = do
    _ <- parseWordForceSpace "make"
    pure MAKE

parseSpeechMark :: Parser TOKENS
parseSpeechMark = do
    _ <- parseCharWithSpace '"'
    pure SPEECHMARK

parseColon :: Parser TOKENS
parseColon = do
    _ <- parseCharWithSpace ':'
    pure COLON

parseVariableUsage :: TokenParser ()
parseVariableUsage = do
    x <- liftToken $ parseMake <|> parseSpeechMark <|> parseColon
    curr <- get
    put $ x:curr

-- | Function Pointers

parseTo :: Parser TOKENS
parseTo = do
    _ <- parseWordForceSpace "to"
    pure TO

parseEnd :: Parser TOKENS
parseEnd = do
    _ <- parseWordWithSpace "end"
    pure END

parseFunctionPointers :: TokenParser ()
parseFunctionPointers = do
    x <- liftToken $ parseTo <|> parseEnd
    curr <- get
    put $ x:curr

-- | Movement
parseForward :: Parser TOKENS
parseForward = do
    _ <- parseWordForceSpace "forward"
    pure FORWARD

parseBack :: Parser TOKENS
parseBack = do
    _ <- parseWordForceSpace "back"
    pure BACK

parseLeft :: Parser TOKENS
parseLeft = do
    _ <- parseWordForceSpace "left"
    pure FORWARD

parseRight :: Parser TOKENS
parseRight = do
    _ <- parseWordForceSpace "right"
    pure FORWARD

parseHome :: Parser TOKENS
parseHome = do
    _ <- parseWordWithSpace "home"
    pure HOME

parseMovement :: TokenParser ()
parseMovement = do
    x <- liftToken $ parseForward <|> parseBack <|>
        parseLeft <|> parseRight <|> parseHome
    curr <- get
    put $ x:curr

-- | Pen

parseSetWidth :: Parser TOKENS
parseSetWidth = do
    _ <- parseWordForceSpace "setwidth"
    pure SETWIDTH

parseSetColor :: Parser TOKENS
parseSetColor = do
    _ <- parseWordForceSpace "setcolor"
    pure SETCOLOR

parsePenUp :: Parser TOKENS
parsePenUp = do
    _ <- parseWordWithSpace "penup"
    pure PENUP

parsePenDown :: Parser TOKENS
parsePenDown = do
    _ <- parseWordWithSpace "pendown"
    pure PENDOWN

parseCLS :: Parser TOKENS
parseCLS = do
    _ <- parseWordWithSpace "clearscreen"
    pure CLS

parsePen :: TokenParser ()
parsePen = do
    x <- liftToken $ parseSetWidth <|> parseSetColor <|>
        parsePenUp <|> parsePenDown <|> parseCLS
    curr <- get
    put $ x:curr

-- Variables/Functions

parseName :: TokenParser ()
parseName = do
    liftToken hspace
    nm <- liftToken $ manyTill alphaNumChar $ lookAhead ( void (
                satisfy (==' ') <|> newline
            ))
    liftToken hspace
    curr <- get
    put $ NAME nm:curr

parseValue :: TokenParser ()
parseValue = do
    val <- liftToken Hurtle.Tokenizer.float
    liftToken hspace
    curr <- get
    put $ VALUE val:curr

-- | Final Parsers

parseTokenCode :: TokenParser ()
parseTokenCode = do
    liftToken parseComment <|> parseControlFlow <|> parseBracket <|> 
        parseCommaOrNewLine <|> parseMaths <|> parseVariableUsage <|>
        parseFunctionPointers <|> parseMovement <|> parsePen <|>
        parseValue <|> parseName

    end <- liftToken atEnd
    unless end parseTokenCode

parseTokens :: TokenParser [TOKENS]
parseTokens = do
    parseTokenCode
    curr <- get
    let res = tail curr
    put $ reverse res
    get
