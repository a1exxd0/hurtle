module Hurtle.CodeGeneration(
    parseHogo
) where

-- | Type we parse from (TOKENS) and into (HogoCode, HogoProgram)
import Hurtle.Types
    ( HogoCode(..),
      HogoParser(HogoParser),
      HogoProgram(..),
      KeyValue(Value, Key),
      ParserT,
      TOKENS(..),
      Variable(..) )

-- | Parsing libraries
import Text.Megaparsec
    ( (<|>),
      optional,
      (<?>),
      atEnd,
      satisfy,
      many,
      MonadParsec(try, eof, lookAhead) )
import Control.Monad.State.Strict 
    ( MonadState(get, put),
      replicateM,
      unless,
      void,
      MonadTrans(lift) )
import Data.Maybe ( isJust )

-- | Container type for variable storage
import Data.Map.Strict as Map ( delete, empty, lookup, insert )


--HELPERS------------------------------------------------------------


-- | Lift a parser from ParserT to HogoParser
liftHogo 
    :: ParserT a        -- ^ Parser to lift
    -> HogoParser a
liftHogo p = HogoParser $ lift p

-- | Try to parse to end
emptyParse :: ParserT ()
emptyParse = do
    parseOptionalNewLine
    _ <- try eof <?> " expected end"
    pure ()

-- | Try to parse to end of a scoped section (function or loop)
emptyScopeParse :: ParserT ()
emptyScopeParse = do
    parseOptionalNewLine
    _ <- lookAhead (void $ matchToken RIGHTBRACKET) <?> "expected end"
    pure ()

-- | Add a HogoCode to the program state
updateCode 
    :: HogoCode        -- ^ Code to add
    -> HogoParser ()
updateCode c = do
    curr <- get
    let updated = curr {code = code curr ++ pure c}
    put updated

-- | Add a procedure to the program state
updateProcedure 
    :: String           -- ^ Name of the procedure
    -> [String]         -- ^ Parameter names
    -> HogoProgram      -- ^ Sub-program
    -> HogoParser ()
updateProcedure name params prog = do
    curr <- get
    let updated = curr {
        procTable = Map.insert 
            name                -- ^ Name of the procedure
            (params, prog)      -- ^ Parameters and sub-program
            (procTable curr)    -- ^ Current procedure table of enclosing scope
        }
    put updated

-- | Add a variable to the program state
updateVariable 
    :: String           -- ^ Name of the variable
    -> Variable         -- ^ Value of the variable
    -> HogoParser ()
updateVariable name var = do
    curr <- get
    let updated = curr {
        varTable = Map.insert 
            name                  -- ^ Name of the variable
            var                   -- ^ Value of the variable
            (varTable curr)       -- ^ Current variable table of enclosing scope
        }
    put updated

-- | Check if a procedure exists
checkProcedureExists 
    :: String               -- ^ Name of the procedure
    -> HogoParser Bool
checkProcedureExists str = do
    curr <- get
    let val = Map.lookup str $ procTable curr
    case val of
        Just _ -> pure True
        Nothing -> pure False

-- | Get the number of parameters a procedure has
getProcedureParamCount 
    :: String                -- ^ Name of the procedure
    -> HogoParser Int
getProcedureParamCount str = do
    curr <- get
    let val = Map.lookup str $ procTable curr
    case val of
        Just (xs, _) -> pure $ length xs
        Nothing -> pure 0

-- | Check if a variable exists
checkVariableExists 
    :: String               -- ^ Name of the variable
    -> HogoParser Bool
checkVariableExists str = do
    curr <- get
    let val = Map.lookup str $ varTable curr
    case val of
        Just _ -> pure True
        Nothing -> pure False

-- | Remove a variable from the program state
removeVariable 
    :: String               -- ^ Name of the variable
    -> HogoParser()
removeVariable name = do
    curr <- get
    let updated = curr {varTable = Map.delete name $ varTable curr}
    put updated

-- | Get the name out of a NAME token.
--   If used in bad context, will fail. This is a helper
extractName :: ParserT String
extractName = do
    tk <- satisfy (== NAME "n/a")
    case tk of
        (NAME name) -> pure name
        _ -> fail "Name expected"

-- | Get the float value out of a VALUE token.
--   If used in bad context, will fail. This is a helper
extractValue :: ParserT Float
extractValue = do
    tk <- satisfy (== VALUE 0.0)
    case tk of
        (VALUE v) -> pure v
        _ -> fail "Value expected"

-- | matches against a token (to avoid ==...)
matchToken 
    :: TOKENS           -- ^ Token to match
    -> ParserT TOKENS
matchToken tk = satisfy (== tk)

-- | Parse a newline token if one exists
parseOptionalNewLine :: ParserT ()
parseOptionalNewLine = do
    _ <- optional $ satisfy (== NEWLINE)
    pure ()

-- | Force a newline or fails
parseForcedNewLine :: ParserT ()
parseForcedNewLine = do
    _ <- satisfy (== NEWLINE) <?> " expected newline"
    pure ()

--PARSING VARIABLES------------------------------------------------------------

-- | Variables can be a name, but can only be used if they have been declared in scope
parseVariableByName :: HogoParser Variable
parseVariableByName = do
    _ <- liftHogo $ matchToken COLON    -- Marker for naming a variable
    name <- liftHogo extractName
    existence <- checkVariableExists name   -- Check if the variable exists
    if existence then do
        pure (Variable $ Key name)    -- Success if it does, return the name in a variable
    else
        liftHogo $ fail $ "Variable " ++ name ++ " doesn't exist"

-- | Variables can always be a value, so parse that
parseVariableByValue :: HogoParser Variable
parseVariableByValue = do
    val <- liftHogo (extractValue <?> " expected value")
    pure (Variable $ Value val)

-- | Variables can be a combination of other variables
parseVariableOp :: HogoParser Variable
parseVariableOp = do 
    op <- liftHogo (parseSum <|> parseDiff <|> parseMul <|> parseDiv <?> " expected operation")
    var1 <- parseVariable
    op var1 <$> parseVariable
    where
        parseSum      = matchToken SUM         >> pure Sum
        parseDiff     = matchToken DIFFERENCE  >> pure Difference
        parseMul      = matchToken MULTIPLY    >> pure Multiply
        parseDiv      = matchToken DIV         >> pure Divide

-- | Parse a variable and return (as standalone variables aren't code)
parseVariable :: HogoParser Variable
parseVariable = do
    parseVariableOp <|> parseVariableByName <|> parseVariableByValue

--PARSING FUNCTION DECLARATIONS------------------------------------------------------------

-- | Parse a procedure variable name (Logo Turtle declares with colons)
parseProcVarName :: ParserT String
parseProcVarName = do
    parseOptionalNewLine
    _ <- try $ matchToken COLON
    extractName

-- | Parse a list of procedure variable names
parseProcVarNames :: ParserT [String]
parseProcVarNames = many parseProcVarName

-- | Parse the program portion of a procedure declaration (after
--   declaring parameters).
parseProgramMarkers 
    :: [String]         -- ^ List of parameter names
    -> HogoParser (     
        HogoProgram,    -- ^ Previous state
        HogoProgram     -- ^ HogoProgram of procedure
        )
parseProgramMarkers params = do
    liftHogo parseOptionalNewLine
    _ <- liftHogo $ satisfy (== LEFTBRACKET)

    -- Store the previous state to return to
    prog :: HogoProgram <- get
    
    -- Create a new HogoProgram with empty variable and procedure tables
    let subProgram = HogoProgram { varTable = Map.empty, procTable = Map.empty, code = [] }

    -- Put the empty program we just made into state
    put subProgram 

    -- Set new variables in new state (default value irrelevant as they are set at runtime)
    mapM_ (\(param, value) -> updateVariable param (Variable (Value value))) (zip params [1..])
    liftHogo parseOptionalNewLine

    -- Parse a new program until end condition is reached
    sub <- parseHogoProcedure

    -- Return the previous state and sub-program
    pure (prog, sub)

-- | Parse a procedure declaration in entirety
parseProcedureDeclaration :: HogoParser ()
parseProcedureDeclaration = do

    -- Anything following 'to' is a procedure
    _ <- liftHogo parseOptionalNewLine
    _ <- liftHogo $ satisfy (== TO)

    -- Get procedure name
    _ <- liftHogo parseOptionalNewLine
    name <- liftHogo (extractName <?> "expected name")

    -- Optional variable parse (for 0-many parameters)
    params <- liftHogo parseProcVarNames
    _ <- liftHogo parseOptionalNewLine

    -- Parse the sub-program and return to original state before declaration
    -- Since a declaration is not a HogoCode call
    (prog, sub) <- parseProgramMarkers params
    put prog
    _ <- liftHogo parseOptionalNewLine
    updateProcedure name params sub



--HOGOCODE PARSERS FOR MOVEMENT AND PEN USE---------------------------------------------

-- | Parse a HogoCode call token that takes no arguments
parseNoArgs :: HogoParser ()
parseNoArgs = do
    liftHogo parseOptionalNewLine
    op <- liftHogo ((parseHome <|> parsePenUp <|> parsePenDown <|>
        parseClearScreen) <?> " expected command with no args")
    liftHogo (parseForcedNewLine <?> "expected new line")
    updateCode op
    where
        -- | Movement
        parseHome        = matchToken HOME      >> pure Home

        -- | Pen Commands
        parsePenUp       = matchToken PENUP     >> pure PenUp
        parsePenDown     = matchToken PENDOWN   >> pure PenDown
        parseClearScreen = matchToken CLS       >> pure ClearScreen

-- | Parse a HogoCode call token that takes 1 argument
parseSingleArg :: HogoParser ()
parseSingleArg = do
    liftHogo parseOptionalNewLine
    op <- liftHogo $ parseForward <|> parseBack <|> parseLeft <|>
        parseRight <|> parseSetWidth <|> parseSetColor
    var <- parseVariable
    liftHogo (parseForcedNewLine <?> "expected new line")

    updateCode $ op var
    where
        -- | Movement
        parseForward    = matchToken FORWARD  >> pure Forward
        parseBack       = matchToken BACK     >> pure Back
        parseLeft       = matchToken LEFT     >> pure GoLeft
        parseRight      = matchToken RIGHT    >> pure GoRight

        -- | Pen Commands
        parseSetWidth   = matchToken SETWIDTH >> pure SetWidth
        parseSetColor   = matchToken SETCOLOR >> pure SetColor

-- | Parse a variable declaration
parseVariableDeclaration :: HogoParser ()
parseVariableDeclaration = do

    -- Tokens to indicate a variable declaration
    _ <- liftHogo $ matchToken MAKE
    _ <- liftHogo $ matchToken SPEECHMARK

    -- Extract name, force newline, and place call into state
    -- So we know when the code's mutable variables change
    name <- liftHogo (extractName <?> " can't get name")
    val <- parseVariable
    liftHogo parseForcedNewLine
    updateVariable name val
    updateCode $ MakeVariable name val

--HOGOCODE CONTROL FLOW CALLS-----------------------------------------------------

-- | Encloses the scope of lines of subprogram, not a standalone HogoProgram as
--   it has access to the enclosing scope, but is marked out by brackets. Therefore
--   any changes made in the for are kept in the state and not isolated.
parseForCapture :: HogoParser [HogoCode]
parseForCapture = do
    _ <- liftHogo (matchToken LEFTBRACKET <?> " expected '[' to start code") 
    liftHogo parseOptionalNewLine

    -- Get current state
    prog :: HogoProgram <- get

    -- Create a new HogoProgram 
    let subProgram = HogoProgram { varTable = varTable prog, procTable = procTable prog, code = [] }

    -- Parse the sub-program code
    put subProgram
    sub <- parseHogoFor

    -- Return to original state and return code in loop
    put prog
    liftHogo parseOptionalNewLine
    pure $ code sub

-- | Parse a for loop
parseFor :: HogoParser ()
parseFor = do
    liftHogo parseOptionalNewLine
    _ <- liftHogo $ matchToken FOR
    liftHogo parseOptionalNewLine

    -- VARIABLE CAPTURE for for loop parameters
    _ <- liftHogo $ matchToken LEFTBRACKET
    liftHogo parseOptionalNewLine
    
    var         <- liftHogo (extractName <?> " expected string for for loop")
    start       <- parseVariable
    end         <- parseVariable
    step        <- parseVariable

    -- End variable capture
    liftHogo parseOptionalNewLine
    _ <- liftHogo $ matchToken RIGHTBRACKET
    liftHogo parseOptionalNewLine

    -- HogoCode capture
    updateVariable var start
    code <- parseForCapture

    -- Add captured code to program state
    removeVariable var
    updateCode $ For var start end step code
    pure ()

-- | Parse a repeat loop
parseRepeat :: HogoParser ()
parseRepeat = do
    liftHogo parseOptionalNewLine
    _ <- liftHogo $ matchToken REPEAT

    -- Capture number of repeats
    liftHogo parseOptionalNewLine
    num <- parseVariable
    liftHogo parseOptionalNewLine

    -- Capture enclosed code
    code <- parseForCapture

    -- Update state with code
    updateCode $ Repeat num code
    pure ()

-- | Parse a function call (jump to function)
parseFunctionCall :: HogoParser ()
parseFunctionCall = do

    -- Get function name and check it exists
    liftHogo parseOptionalNewLine
    name <- liftHogo extractName
    existence <- checkProcedureExists name

    -- Get number of parameters
    paramCount <- getProcedureParamCount name

    -- Fail if it doesn't exist
    if existence then do

        -- Parse a list of parameters to the same length as declaration
        params <- replicateM paramCount parseVariable
        liftHogo parseOptionalNewLine

        -- Wrong number of parameters
        if length params /= paramCount
        then liftHogo $ fail $ "Expected " 
            ++ show paramCount ++ " params but got " ++ show (length params)

        -- Store function call if no issues
        else updateCode $ Function name params
    else
        liftHogo $ fail $ "Function " ++ name ++ " doesn't exist"
        

--ALTERNATIVE END STATEMENTS-------------------------------------------

-- | Parse a RIGHTBRACKET token if one exists and force a procedure end.
--   Returns false with no fail if there isn't a right bracket token.
parseProcedureEnd :: ParserT Bool
parseProcedureEnd = do

    -- Check if we are at the end of procedure
    parseOptionalNewLine
    rB <- optional (matchToken RIGHTBRACKET <?> " expected ']'")
    parseOptionalNewLine
    if isJust rB then do

        -- Then we expect the END token to follow a close bracket
        _ <- matchToken END <?> " expected 'end'"
        _ <- parseForcedNewLine
        pure True
    else

        -- Return false if we aren't able to parse a closing brace
        pure False

-- | Parse a RIGHTBRACKET token if one exists
parseForEnd :: ParserT Bool
parseForEnd = do
    parseOptionalNewLine
    rB <- optional (matchToken RIGHTBRACKET <?> " expected ']'")
    pure $ isJust rB

--RUNNING POINTS------------------------------------------------------

-- | Parse an option of HogoCodes that are parseable
parserCombination :: HogoParser ()
parserCombination = do
    parseProcedureDeclaration <|> parseVariableDeclaration <|>
        parseNoArgs <|> parseSingleArg <|> parseFor <|> parseRepeat <|>
        parseFunctionCall <|> liftHogo parseOptionalNewLine  <|> liftHogo emptyParse

-- | Parse hogocodes and force a for loop end condition
parseHogoCodeFor :: HogoParser ()
parseHogoCodeFor = do
    parserCombination <|> liftHogo emptyScopeParse

    end <- liftHogo (try parseForEnd <?> " end ']' expected")
    unless end parseHogoCodeFor

-- | Parse hogocodes and force a procedure end condition
parseHogoCodeProcedure :: HogoParser ()
parseHogoCodeProcedure = do
    parserCombination <|> liftHogo emptyScopeParse

    endCheck <- liftHogo (try parseProcedureEnd <?> " end ']' expected")
    unless endCheck parseHogoCodeProcedure
 
-- | Standard hogocode parser will check through and append to code of program.
--   
--   If we arent in a loop or procedure, we shouldn't expect stray brackets, so
--   if this is the case, fail the parser.
parseHogoCode :: HogoParser ()
parseHogoCode = do
    parserCombination

    isStandaloneBracket <- liftHogo $ optional (matchToken LEFTBRACKET <|> matchToken RIGHTBRACKET) 
    case isStandaloneBracket of
        Just x -> liftHogo $ fail $ "bad " ++ show x ++ " token" 
        Nothing -> do
            end <- liftHogo (atEnd <?> " end expected")
            unless end parseHogoCode

-- | Starting point for a for loop (or repeat) parse
parseHogoFor :: HogoParser HogoProgram
parseHogoFor = do
    parseHogoCodeFor
    get

-- | Starting point for a procedure declaration parse
parseHogoProcedure :: HogoParser HogoProgram
parseHogoProcedure = do
    parseHogoCodeProcedure
    get

-- | Starting point for the program. This can be called by setting an
--   initial (blank optional) HogoProgram and loading it into the parser
--   as follows:
--
--   @runParser (runStateT (runHogoParser parseHogo) initialState) "" tokenState@
--
--   Where inititialState is any HogoProgram, and tokenState is the state of what
--   list of tokens you want to parse.
parseHogo :: HogoParser HogoProgram
parseHogo = do
    parseHogoCode
    get


