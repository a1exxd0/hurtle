module Hurtle.CodeGeneration(
    parseHogo
) where

import Hurtle.Types
import Text.Megaparsec
import Data.Map.Strict as Map
import Control.Monad.State.Strict
import Data.Maybe

-- | Helpers

liftHogo :: ParserT a -> HogoParser a
liftHogo p = HogoParser $ lift p

emptyParse :: ParserT ()
emptyParse = do
    parseOptionalNewLine
    _ <- eof <?> " expected end"
    pure ()

emptyScopeParse :: ParserT ()
emptyScopeParse = do
    parseOptionalNewLine
    _ <- lookAhead (void $ matchToken RIGHTBRACKET) <?> "expected end"
    pure ()

parseThrow :: ParserT ()
parseThrow = do
    _ <- satisfy (const True)
    fail "no"

updateCode :: HogoCode -> HogoParser ()
updateCode c = do
    curr <- get
    let updated = curr {code = code curr ++ pure c}
    put updated

updateProcedure :: String -> [String] -> HogoProgram -> HogoParser ()
updateProcedure name params prog = do
    curr <- get
    let updated = curr {procTable = Map.insert name (params, prog) (procTable curr)}
    put updated

updateVariable :: String -> Variable -> HogoParser ()
updateVariable name var = do
    curr <- get
    let updated = curr {varTable = Map.insert name var (varTable curr)}
    put updated

checkProcedureExists :: String -> HogoParser Bool
checkProcedureExists str = do
    curr <- get
    let val = Map.lookup str (procTable curr)
    case val of
        Just _ -> pure True
        Nothing -> pure False

getProcedureParamCount :: String -> HogoParser Int
getProcedureParamCount str = do
    curr <- get
    let val = Map.lookup str (procTable curr)
    case val of
        Just (xs, _) -> pure $ length xs
        Nothing -> pure 0

checkVariableExists :: String -> HogoParser Bool
checkVariableExists str = do
    curr <- get
    let val = Map.lookup str (varTable curr)
    case val of
        Just _ -> pure True
        Nothing -> pure False

removeVariable :: String -> HogoParser()
removeVariable name = do
    curr <- get
    let updated = curr {varTable = Map.delete name $ varTable curr}
    put updated

extractName :: ParserT String
extractName = do
    tk <- satisfy (== NAME "n/a")
    case tk of
        (NAME name) -> pure name
        _ -> fail "Name expected"

extractValue :: ParserT Float
extractValue = do
    tk <- satisfy (== VALUE 0.0)
    case tk of
        (VALUE v) -> pure v
        _ -> fail "Value expected"

matchToken :: TOKENS -> ParserT TOKENS
matchToken tk = do satisfy (== tk)

parseOptionalNewLine :: ParserT ()
parseOptionalNewLine = do
    _ <- optional $ satisfy (== NEWLINE)
    pure ()

parseForcedNewLine :: ParserT ()
parseForcedNewLine = do
    _ <- satisfy (== NEWLINE) <?> " expected newline"
    pure ()

-- | Variable Parsing

parseVariableByName :: HogoParser Variable
parseVariableByName = do
    _ <- liftHogo $ matchToken COLON
    name <- liftHogo extractName
    existence <- checkVariableExists name
    if existence then do
        pure (Variable $ Key name)
    else
        liftHogo $ fail $ "Variable " ++ name ++ " doesn't exist"

parseVariableByValue :: HogoParser Variable
parseVariableByValue = do
    val <- liftHogo (extractValue <?> "expected value")
    pure (Variable $ Value val)

parseVariableOp :: HogoParser Variable
parseVariableOp = do 
    op <- liftHogo (parseSum <|> parseDiff <|> parseMul <?> " expected operation")
    var1 <- parseVariable
    op var1 <$> parseVariable
    where
        parseSum      = matchToken SUM         >> pure Sum
        parseDiff     = matchToken DIFFERENCE  >> pure Difference
        parseMul      = matchToken MULTIPLY    >> pure Multiply

parseVariable :: HogoParser Variable
parseVariable = do
    parseVariableOp <|> parseVariableByName <|> parseVariableByValue

-- | Variable Declaration
parseVariableDeclaration :: HogoParser ()
parseVariableDeclaration = do
    _ <- liftHogo $ matchToken MAKE
    _ <- liftHogo $ matchToken SPEECHMARK
    name <- liftHogo (extractName <?> " can't get name")
    val <- parseVariable
    liftHogo parseForcedNewLine
    updateVariable name val
    updateCode $ MakeVariable name val

-- | Function Declaration

parseProcVarName :: ParserT String
parseProcVarName = do
    parseOptionalNewLine
    _ <- try $ matchToken COLON
    extractName

parseProcVarNames :: ParserT [String]
parseProcVarNames = do many parseProcVarName

parseProgramMarkers :: [String] -> HogoParser (HogoProgram, HogoProgram)
parseProgramMarkers params = do
    liftHogo parseOptionalNewLine
    _ <- liftHogo $ satisfy (== LEFTBRACKET)
    prog :: HogoProgram <- get
    -- Create a new HogoProgram with empty variable and procedure tables
    let subProgram = HogoProgram { varTable = Map.empty, procTable = Map.empty, code = [] }
    -- Parse the sub-program code
    put subProgram
    liftHogo parseOptionalNewLine
    sub <- parseHogoWithParams params
    -- Get the final state (sub-program with parsed code)
    -- Return the sub-program
    pure (prog, sub)

-- FINAL USE
parseProcedureDeclaration :: HogoParser ()
parseProcedureDeclaration = do
    _ <- liftHogo parseOptionalNewLine
    _ <- liftHogo $ satisfy (== TO)

    _ <- liftHogo parseOptionalNewLine
    name <- liftHogo (extractName <?> "expected name")

    params <- liftHogo parseProcVarNames
    _ <- liftHogo parseOptionalNewLine

    (prog, sub) <- parseProgramMarkers params
    put prog
    _ <- liftHogo parseOptionalNewLine
    updateProcedure name params sub

-- | HogoCode parsers (non-control flow)
parseNoArgs :: HogoParser ()
parseNoArgs = do
    liftHogo parseOptionalNewLine
    op <- liftHogo ((parseHome <|> parsePenUp <|> parsePenDown <|>
        parseClearScreen) <?> " expected command with no args")
    liftHogo (parseForcedNewLine <?> "expected new line")
    updateCode op
    where
        parseHome        = matchToken HOME      >> pure Home
        parsePenUp       = matchToken PENUP     >> pure PenUp
        parsePenDown     = matchToken PENDOWN   >> pure PenDown
        parseClearScreen = matchToken CLS       >> pure ClearScreen

parseSingleArg :: HogoParser ()
parseSingleArg = do
    liftHogo parseOptionalNewLine
    op <- liftHogo $ parseForward <|> parseBack <|> parseLeft <|>
        parseRight <|> parseSetWidth <|> parseSetColor
    var <- parseVariable
    liftHogo (parseForcedNewLine <?> "expected new line")

    updateCode $ op var
    where
        parseForward    = matchToken FORWARD  >> pure Forward
        parseBack       = matchToken BACK     >> pure Back
        parseLeft       = matchToken LEFT     >> pure GoLeft
        parseRight      = matchToken RIGHT    >> pure GoRight
        parseSetWidth   = matchToken SETWIDTH >> pure SetWidth
        parseSetColor   = matchToken SETCOLOR >> pure SetColor

-- CONTROL FLOW
parseForCapture :: HogoParser [HogoCode]
parseForCapture = do
    _ <- liftHogo (matchToken LEFTBRACKET <?> " expected '[' to start code") 
    liftHogo parseOptionalNewLine
    code <- parseHogoFor
    liftHogo parseOptionalNewLine
    pure code

parseFor :: HogoParser ()
parseFor = do
    liftHogo parseOptionalNewLine
    _ <- liftHogo $ matchToken FOR
    liftHogo parseOptionalNewLine

    -- | VARIABLE CAPTURE
    _ <- liftHogo $ matchToken LEFTBRACKET
    liftHogo parseOptionalNewLine
    var <- liftHogo (extractName <?> " expected string for for loop")
    start <- parseVariable
    end <- parseVariable
    step <- parseVariable
    liftHogo parseOptionalNewLine
    _ <- liftHogo $ matchToken RIGHTBRACKET
    liftHogo parseOptionalNewLine

    -- | PROCEDURE CAPTURE
    updateVariable var start
    code <- parseForCapture

    -- | END
    removeVariable var
    updateCode $ For var start end step code
    pure ()

parseRepeat :: HogoParser ()
parseRepeat = do
    liftHogo parseOptionalNewLine
    _ <- liftHogo $ matchToken REPEAT

    liftHogo parseOptionalNewLine
    num <- liftHogo (extractValue <?> " expected end for repeat ")
    liftHogo parseOptionalNewLine

    code <- parseForCapture

    updateCode $ Repeat (truncate num) code
    pure ()

parseFunctionCall :: HogoParser ()
parseFunctionCall = do
    liftHogo parseOptionalNewLine
    name <- liftHogo extractName
    existence <- checkProcedureExists name
    paramCount <- getProcedureParamCount name
    if existence then do
        params <- replicateM paramCount parseVariable
        liftHogo parseOptionalNewLine
        if length params /= paramCount
        then liftHogo $ fail $ "Expected " ++ show paramCount ++ " params but got " ++ show (length params)
        else updateCode $ Function name params
    else
        liftHogo $ fail $ "Function " ++ name ++ " doesn't exist"
        

-- NOT START POINT \/

parseProcedureEnd :: ParserT Bool
parseProcedureEnd = do
    parseOptionalNewLine
    rB <- optional (matchToken RIGHTBRACKET <?> " expected ']'")
    parseOptionalNewLine
    if isJust rB then do
        _ <- matchToken END <?> " expected 'end'"
        _ <- parseForcedNewLine
        pure True
    else
        pure False

parseHogoCodeWithParams :: HogoParser ()
parseHogoCodeWithParams = do
    parserCombination <|> liftHogo emptyScopeParse

    endCheck <- liftHogo (try parseProcedureEnd <?> " end ']' expected")
    unless endCheck parseHogoCodeWithParams

parseHogoWithParams :: [String] -> HogoParser HogoProgram
parseHogoWithParams params = do
    mapM_ (\(param, value) -> updateVariable param (Variable (Value value))) (zip params [1..])
    parseHogoCodeWithParams
    get

parseForEnd :: ParserT Bool
parseForEnd = do
    parseOptionalNewLine
    rB <- optional (matchToken RIGHTBRACKET <?> " expected ']'")
    pure $ isJust rB

parseForCodeHelper :: HogoParser ()
parseForCodeHelper = do
    parserCombination <|> liftHogo emptyScopeParse

    end <- liftHogo (try parseForEnd <?> " end ']' expected")
    unless end parseForCodeHelper

parseForCode :: HogoParser HogoProgram
parseForCode = do
    parseForCodeHelper
    get

parseHogoFor :: HogoParser [HogoCode]
parseHogoFor = do
    prog :: HogoProgram <- get
    -- Create a new HogoProgram 
    let subProgram = HogoProgram { varTable = varTable prog, procTable = procTable prog, code = [] }
    -- Parse the sub-program code
    put subProgram
    sub <- parseForCode
    put prog
    pure $ code sub

-- | Parsers

parserCombination :: HogoParser ()
parserCombination = do
    parseProcedureDeclaration <|> parseVariableDeclaration <|>
        parseNoArgs <|> parseSingleArg <|> parseFor <|> parseRepeat <|>
        parseFunctionCall <|> liftHogo parseOptionalNewLine  <|> liftHogo emptyParse
 

parseHogoCode :: HogoParser ()
parseHogoCode = do
    parserCombination

    isStandaloneBracket <- liftHogo $ optional (matchToken LEFTBRACKET <|> matchToken RIGHTBRACKET) 
    case isStandaloneBracket of
        Just x -> liftHogo $ fail $ "bad " ++ show x ++ " token" 
        Nothing -> do
            end <- liftHogo (atEnd <?> " end expected")
            unless end parseHogoCode

parseHogo :: HogoParser HogoProgram
parseHogo = do
    parseHogoCode
    get


