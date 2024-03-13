module Hurtle.Parser where

import Hurtle.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Char
import Data.Map.Strict as Map
import Control.Monad.State.Strict
import Data.Maybe
{-

-- | Lifts a @Parser@ into a @HogoParser@
liftHogo :: Parser a -> HogoParser a
liftHogo p = HogoParser $ lift p

-- | Parses the end of file if required
parseToEOF :: Parser ()
parseToEOF = do
    _ <- try eof
    pure ()

-- | Parses a newline character
parseNL :: Parser ()
parseNL = do 
    _ <- hspace
    _ <- satisfy (=='\n') <|> satisfy (==',') <|> satisfy (==']')
    pure ()

parseToNewLine :: Parser ()
parseToNewLine = parseToEOF <|> parseNL

float :: Parser Float
float = do
    integerPart <- some digitChar
    decimalPart <- optional $ char '.' *> some digitChar
    let value = case decimalPart of
            Nothing -> read integerPart
            Just dPart -> read (integerPart ++ "." ++ dPart)
    pure value

-- | Absorb comments
parseComment :: HogoParser ()
parseComment = do
    liftHogo hspace
    _ <- liftHogo $ satisfy (==';')
    _ <- liftHogo $ manyTill anySingle $ lookAhead ( void (
            satisfy (== '\n')
        ))
    liftHogo hspace
    _ <- liftHogo parseToNewLine
    pure ()

-- | Update code

updateCode :: HogoCode -> HogoParser ()
updateCode c = do
    curr <- get
    let updated = curr {code = code curr ++ pure c}
    put updated

-- | Variable manipulations

updateVariable :: String -> Variable -> HogoParser ()
updateVariable name var = do
    curr <- get
    let updated = curr {varTable = Map.insert name var (varTable curr)}
    put updated

checkVariableExists :: String -> HogoParser Bool
checkVariableExists str = do
    curr <- get
    let val = Map.lookup str (varTable curr)
    case val of
        Just _ -> pure True
        Nothing -> pure False

-- | Variable Parsers

parseVariableByValue :: HogoParser Variable
parseVariableByValue = do 
    liftHogo hspace >> Variable . Value <$> liftHogo float

parseVariableByName :: HogoParser Variable
parseVariableByName = do
    liftHogo hspace
    _ <- liftHogo $ satisfy (==':')
    varName <- liftHogo $ lookAhead $ manyTill alphaNumChar  ( void (
                satisfy (\c -> c == ' ' || c == ',' || c == '\n')
            ))
    existence <- checkVariableExists varName
    if existence then do
        _ <- liftHogo $ manyTill alphaNumChar ( void (
                satisfy (\c -> c == ' ' || c == ',' || c == '\n')
            ))
        pure (Variable $ Key varName)
    else
        liftHogo $ fail "Non-existent variable used"

parseVariableOp :: HogoParser Variable
parseVariableOp = do 
    liftHogo hspace
    op <- liftHogo (parseSum <|> parseDiff <|> parseMul)
    liftHogo hspace
    var1 <- parseVariable
    liftHogo hspace
    op var1 <$> parseVariable
    where
        parseSum      = chunk "sum"         >> pure Sum
        parseDiff     = chunk "difference"  >> pure Difference
        parseMul      = chunk "multiply"    >> pure Multiply

parseVariable :: HogoParser Variable
parseVariable = parseVariableByName <|> parseVariableByValue <|> parseVariableOp

-- | Variable Declaration Parser

parseVariableDeclaration :: HogoParser ()
parseVariableDeclaration = do
    liftHogo hspace
    _ <- liftHogo $ chunk "make"
    liftHogo hspace
    _ <- liftHogo $ satisfy (=='"')
    varName <- liftHogo $ manyTill alphaNumChar $ lookAhead ( void (
                satisfy (==' ')
            ))
    liftHogo hspace
    val <- parseVariable
    liftHogo hspace
    _ <- liftHogo parseToNewLine
    updateVariable varName val

-- | Procedure manipulations

updateProcedure :: String -> [String] -> HogoProgram -> HogoParser ()
updateProcedure name params prog = do
    curr <- get
    let updated = curr {procTable = Map.insert name (params, prog) (procTable curr)}
    put updated

getVariableName :: Parser String
getVariableName = do
    hspace
    _ <- satisfy (== ':')
    manyTill alphaNumChar
        (void (satisfy (\ c -> c == ' ' || c == '\n' || c == ']')))

checkProcedureExists :: String -> HogoParser Bool
checkProcedureExists str = do
    curr <- get
    let val = Map.lookup str (procTable curr)
    case val of
        Just _ -> pure True
        Nothing -> pure False

-- | Procedure Call Parsers

parseProcedureCall :: HogoParser String
parseProcedureCall = undefined

-- | Procedure Declaration Parsers

parseProgramMarkers :: [String] -> HogoParser (HogoProgram, HogoProgram)
parseProgramMarkers params = do
    liftHogo hspace
    _ <- liftHogo $ satisfy (=='[')
    liftHogo space
    prog :: HogoProgram <- get
    -- Create a new HogoProgram with empty variable and procedure tables
    let subProgram = HogoProgram { varTable = Map.empty, procTable = Map.empty, code = [] }
    -- Parse the sub-program code
    put subProgram
    sub <- parseHogoWithParams params
    -- Get the final state (sub-program with parsed code)
    -- Return the sub-program
    pure (prog, sub)

parseProcedureEnd :: HogoParser Bool
parseProcedureEnd = do
    liftHogo hspace
    -- Attempt to parse "end"
    isEnd <- optional $ liftHogo $ chunk "end"
    liftHogo hspace
    -- Return True if "end" was parsed, False otherwise
    pure $ case isEnd of
        Just _ -> True
        Nothing -> False

parseParam :: Parser String
parseParam = do
    hspace
    name <- getVariableName
    hspace
    pure name

parseParams :: HogoParser [String]
parseParams = do 
    liftHogo hspace
    liftHogo $ many parseParam


parseProcedureDeclaration :: HogoParser ()
parseProcedureDeclaration = do
    liftHogo hspace
    _ <- liftHogo $ chunk "to"
    liftHogo hspace
    procName <- liftHogo $ manyTill alphaNumChar $ lookAhead ( void (
                satisfy (==' ')
            ))
    params <- parseParams
    (prog, sub) <- parseProgramMarkers params
    put prog
    updateProcedure procName params sub


-- | HogoCode Parsers

parseNoArgs :: HogoParser ()
parseNoArgs = do
    op <- liftHogo (parseHome <|> parsePenUp <|> parsePenDown)
    _ <- liftHogo hspace
    _ <- liftHogo parseToNewLine
    updateCode op
    where
        parseHome       = chunk "home"      >> pure Home
        parsePenUp      = chunk "penup"     >> pure PenUp
        parsePenDown    = chunk "pendown"   >> pure PenDown


parseSingleArg :: HogoParser ()
parseSingleArg = do
    liftHogo hspace
    op <- liftHogo ( 
        parseForward    <|> parseBackward <|> parseLeft <|>
        parseRight      <|> parseSetWidth <|> parseSetColor )
    _ <- liftHogo hspace
    var <- parseVariable
    _ <- liftHogo hspace
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
    parseNoArgs <|> parseSingleArg              <|> parseVariableDeclaration 
                <|> liftHogo parseToNewLine     <|> parseComment
                <|> parseProcedureDeclaration

    end <- liftHogo atEnd <|> parseProcedureEnd
    unless end parseHogoCode

parseHogoCodeWithParams :: HogoParser ()
parseHogoCodeWithParams = do
    parseNoArgs <|> parseSingleArg              <|> parseVariableDeclaration 
                <|> liftHogo parseToNewLine     <|> parseComment
                <|> parseProcedureDeclaration

    end <- parseProcedureEnd
    unless end parseHogoCode

-- | HogoProgram Parsers

parseHogo :: HogoParser HogoProgram
parseHogo = do
    parseHogoCode
    get

parseHogoWithParams :: [String] -> HogoParser HogoProgram
parseHogoWithParams params = do
    mapM_ (\(param, value) -> updateVariable param (Variable (Value value))) (zip params [1..])
    parseHogoCodeWithParams
    get

    -}