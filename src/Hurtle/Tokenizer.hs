module Hurtle.Tokenizer(
    parseTokens -- ^ This function takes a string and returns a list of tokens
) where

-- | Result of parse type
import Hurtle.Types
    ( TokenParser(TokenParser), Parser, TOKENS(..) )

-- | Parsing
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
    ( alphaNumChar, char, digitChar, hspace, hspace1, newline, space )
import Data.Char ()
import Data.Maybe ()
import Text.Megaparsec.Char.Lexer ( skipLineComment )

-- | State import
import Control.Monad.State.Strict
    ( unless, void, MonadState(get, put), MonadTrans(lift) )


--HELPERS------------------------------------------------------------

-- | Takes a parser and lifts it into the TokenParser monad.
liftToken 
    :: Parser a             -- ^ Parser to lift
    -> TokenParser a
liftToken p = TokenParser $ lift p

-- | Takes no space and tries to parse to end of file
parseToEOFNoSpace :: Parser ()
parseToEOFNoSpace = do
    _ <- try eof
    pure ()

-- | Takes optional space and tries to parse to end of file
parseToEOFWithSpace :: Parser ()
parseToEOFWithSpace = do
    hspace
    parseToEOFNoSpace

-- | Use a parser wrapped with optional space either side
parseWithSpace 
    :: Parser a          -- ^ Parser to use
    -> Parser ()         -- ^ Void parser return because I don't want result
parseWithSpace ps = do
    hspace
    _ <- ps
    hspace
    pure ()

-- | Parse a specific character (with optional space)
parseCharWithSpace 
    :: Char              -- ^ Character to parse
    -> Parser ()
parseCharWithSpace c = do
    parseWithSpace $ satisfy (==c)

-- | Parse a specific word (with optional space)
parseWordWithSpace 
    :: String            -- ^ Word to parse
    -> Parser ()
parseWordWithSpace word = do
    parseWithSpace $ chunk word

-- | Parse a specific word (with forced space)
parseWordForceSpace 
    :: String           -- ^ Word to parse
    -> Parser ()
parseWordForceSpace word = do
    hspace
    _ <- chunk word
    hspace1 <|> try eof -- no space exists if we are at the end. satisfy

-- | Comment parsing on semicolon detection, ignores rest of line to newline
parseComment :: Parser ()
parseComment = do
    hspace
    _ <- skipLineComment ";" -- stops before newline
    pure ()

-- | Float parsing, returns value. Does not support negatives (-...)
float :: Parser Float
float = do
    hspace
    integerPart <- some digitChar
    decimalPart <- optional $ char '.' *> some digitChar
    let value = case decimalPart of
            Nothing -> read integerPart
            Just dPart -> read (integerPart ++ "." ++ dPart)
    pure value

--CONTROL FLOW KEYWORDS-------------------------------------------------

-- | Parse a for statement and force a space at the end of the keyword
parseFor :: Parser TOKENS
parseFor = do
    _ <- try $ parseWordForceSpace "for"
    pure FOR

-- | Parse a repeat statement and force a space at the end of the keyword
parseRepeat :: Parser TOKENS
parseRepeat = do
    _ <- try $ parseWordForceSpace "repeat"
    pure REPEAT

-- | Control flow keyword parsing, puts keyword into [TOKENS] state
parseControlFlow :: TokenParser ()
parseControlFlow = do
    x <- liftToken $ parseFor <|> parseRepeat
    curr <- get
    put (x:curr)

--PARSE BRACKETS---------------------------------------------------------

-- | Parse a right bracket with optional space
parseRightBracket :: Parser TOKENS
parseRightBracket = do
    _ <- parseCharWithSpace ']'
    pure RIGHTBRACKET

-- | Parse a left bracket with optional space
parseLeftBracket :: Parser TOKENS
parseLeftBracket = do
    _ <- parseCharWithSpace '['
    pure LEFTBRACKET

-- | Parse brackets, puts bracket into [TOKENS] state
parseBracket :: TokenParser ()
parseBracket = do
    x <- liftToken $ parseLeftBracket <|> parseRightBracket
    curr <- get
    if x == RIGHTBRACKET then           -- robustness newline insert for code generation
        if head curr == NEWLINE 
        then put $ x:curr
        else put $ x:NEWLINE:curr
    else put $ x:curr

-- | Markers

-- | Parse a comma or newline with optional space. Directly puts
--   the token into the state, only if one isn't present already
--   , i.e. max newlines is 1 in a row at any time.
--
--   This is to ensure that the code generation is robust, treating
--   newlines as a single newline, or multiple commas as a single
--   newline too.
parseCommaOrNewLine :: TokenParser ()
parseCommaOrNewLine = do
    _ <- liftToken $ optional $ satisfy (=='\r')
    _ <- liftToken $ parseWithSpace (satisfy (==',') <|> newline)
    curr <- get
    if null curr then put [NEWLINE]
    else if head curr == NEWLINE 
    then put curr
    else put $ NEWLINE:curr

--MATH OPERATORS-------------------------------------------------------

-- | Parse a sum keyword and force a space at the end of the keyword,
--   so that if someone declares a variable sumTwo, it doesn't get
--   confused with the keyword.
parseSum :: Parser TOKENS
parseSum = do
    _ <- parseWordForceSpace "sum"
    pure SUM

-- | Parse a difference keyword and force a space at the end of the keyword
parseDifference :: Parser TOKENS
parseDifference = do
    _ <- parseWordForceSpace "difference"
    pure DIFFERENCE

-- | Parse a multiply keyword and force a space at the end of the keyword
parseMultiply :: Parser TOKENS
parseMultiply = do
    _ <- parseWordForceSpace "multiply"
    pure MULTIPLY

-- | Parse a divide keyword and force a space at the end of the keyword
parseDiv :: Parser TOKENS
parseDiv = do
    _ <- parseWordForceSpace "div"
    pure DIV

-- | Maths parsing, puts keyword into [TOKENS] state
parseMaths :: TokenParser ()
parseMaths = do
    x <- liftToken $ parseSum <|>
        parseDifference <|> parseMultiply <|> parseDiv
    curr <- get
    put $ x:curr

--VARIABLE CREATION & USE-------------------------------------------

-- | Parse a make keyword and force a space at the end of the keyword
parseMake :: Parser TOKENS
parseMake = do
    _ <- parseWordForceSpace "make"
    pure MAKE

-- | Parse a speechmark with optional space
parseSpeechMark :: Parser TOKENS
parseSpeechMark = do
    _ <- parseCharWithSpace '"'
    pure SPEECHMARK

-- | Parse a colon with optional space
parseColon :: Parser TOKENS
parseColon = do
    _ <- parseCharWithSpace ':'
    pure COLON

-- | Variable usage parsing, puts keyword into [TOKENS] state
--   Syntax is not checked here, we parse whatever whenever.
parseVariableUsage :: TokenParser ()
parseVariableUsage = do
    x <- liftToken $ parseMake <|> parseSpeechMark <|> parseColon
    curr <- get
    put $ x:curr

--FUNCTION POINTERS---------------------------------------------------

-- | Parse a to keyword and force a space at the end of the keyword
parseTo :: Parser TOKENS
parseTo = do
    _ <- parseWordForceSpace "to"
    pure TO

-- | Parse an end keyword with optional space
parseEnd :: Parser TOKENS
parseEnd = do
    _ <- parseWordWithSpace "end"
    pure END

-- | Function pointer parsing, puts keyword into [TOKENS] state
parseFunctionPointers :: TokenParser ()
parseFunctionPointers = do
    x <- liftToken $ parseTo <|> parseEnd
    curr <- get
    put $ x:curr

--MOVEMENT-----------------------------------------------------------

-- | Parse a forward keyword and force a space at the end of the keyword
parseForward :: Parser TOKENS
parseForward = do
    _ <- parseWordForceSpace "forward"
    pure FORWARD

-- | Parse a back keyword and force a space at the end of the keyword
parseBack :: Parser TOKENS
parseBack = do
    _ <- parseWordForceSpace "back"
    pure BACK

-- | Parse a left keyword and force a space at the end of the keyword
parseLeft :: Parser TOKENS
parseLeft = do
    _ <- parseWordForceSpace "left"
    pure LEFT

-- | Parse a right keyword and force a space at the end of the keyword
parseRight :: Parser TOKENS
parseRight = do
    _ <- parseWordForceSpace "right"
    pure RIGHT

-- | Parse a home keyword and force a space at the end of the keyword
parseHome :: Parser TOKENS
parseHome = do
    _ <- parseWordWithSpace "home"
    pure HOME

-- | Movement parsing, puts keyword into [TOKENS] state
parseMovement :: TokenParser ()
parseMovement = do
    x <- liftToken $ parseForward <|> parseBack <|>
        parseLeft <|> parseRight <|> parseHome
    curr <- get
    put $ x:curr

--PEN COMMANDS--------------------------------------------------------

-- | Parse a setwidth keyword and force a space at the end of the keyword
parseSetWidth :: Parser TOKENS
parseSetWidth = do
    _ <- parseWordForceSpace "setwidth"
    pure SETWIDTH

-- | Parse a setcolor keyword and force a space at the end of the keyword
parseSetColor :: Parser TOKENS
parseSetColor = do
    _ <- parseWordForceSpace "setcolor"
    pure SETCOLOR

-- | Parse a penup keyword and force a space at the end of the keyword
parsePenUp :: Parser TOKENS
parsePenUp = do
    _ <- parseWordWithSpace "penup"
    pure PENUP

-- | Parse a pendown keyword and force a space at the end of the keyword
parsePenDown :: Parser TOKENS
parsePenDown = do
    _ <- parseWordWithSpace "pendown"
    pure PENDOWN

-- | Parse a clearscreen keyword and force a space at the end of the keyword
parseCLS :: Parser TOKENS
parseCLS = do
    _ <- parseWordWithSpace "clearscreen"
    pure CLS

-- | Pen command parsing, puts keyword into [TOKENS] state
parsePen :: TokenParser ()
parsePen = do
    x <- liftToken $ parseSetWidth <|> parseSetColor <|>
        parsePenUp <|> parsePenDown <|> parseCLS
    curr <- get
    put $ x:curr

--VARIABLES/FUNCTIONS---------------------------------------------------

-- | Parse a name and put it into the state
parseName :: TokenParser ()
parseName = do
    liftToken hspace
    nm <- liftToken $ manyTill alphaNumChar $ lookAhead ( void (
                satisfy (==' ') <|> 
                newline <|> 
                lookAhead (satisfy (==']')) <|> 
                lookAhead (satisfy (==','))
            ))
    liftToken hspace
    curr <- get
    put $ NAME nm:curr

-- | Parse a value and put it into the state
parseValue :: TokenParser ()
parseValue = do
    val <- liftToken Hurtle.Tokenizer.float
    liftToken hspace
    curr <- get
    put $ VALUE val:curr

--FINAL PARSERS--------------------------------------------------------

-- | Parse a token, and if it fails, it will try the next parser
parseTokenCode :: TokenParser ()
parseTokenCode = do

    -- | Note the order that name is parsed last because anything would satisfy this
    liftToken parseComment <|> parseMovement <|> parseBracket <|> 
        parseCommaOrNewLine <|> parseMaths <|> parseVariableUsage <|>
        parseFunctionPointers <|> parseControlFlow <|> parsePen <|>
        parseValue <|> parseName <|> liftToken parseToEOFNoSpace <|> liftToken parseToEOFWithSpace

    end <- liftToken atEnd
    unless end parseTokenCode

-- | Point to run runParser from, returns @[TOKENS]@ from input string
parseTokens :: TokenParser [TOKENS]
parseTokens = do
    liftToken space
    parseTokenCode
    curr <- get
    -- | inserted through cons, so must reverse
    let tks = reverse curr
    case tks of
        (NEWLINE:xs) -> put xs
        _ -> put tks
    get

