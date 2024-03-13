module Hurtle.CodeGeneration where

import Hurtle.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Char
import Data.Map.Strict as Map
import Control.Monad.State.Strict
import Data.Maybe
import Hurtle.Types (TokenParser)

-- | Helpers

liftHogo :: ParserT a -> HogoParser a
liftHogo p = HogoParser $ lift p

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

extractName :: HogoParser String
extractName = do
    tk <- liftHogo $ satisfy (== NAME "n/a")
    case tk of
        (NAME name) -> pure name
        _ -> liftHogo $ fail "Name expected"

-- | Function Set

parseProgramMarkers :: [String] -> HogoParser (HogoProgram, HogoProgram)
parseProgramMarkers params = do
    _ <- liftHogo $ satisfy (== LEFTBRACKET)
    prog :: HogoProgram <- get
    -- Create a new HogoProgram with empty variable and procedure tables
    let subProgram = HogoProgram { varTable = Map.empty, procTable = Map.empty, code = [] }
    -- Parse the sub-program code
    put subProgram
    sub <- parseHogoWithParams params
    -- Get the final state (sub-program with parsed code)
    -- Return the sub-program
    pure (prog, sub)

parseProcedureDeclaration :: HogoParser ()
parseProcedureDeclaration = do
    _ <- liftHogo $ satisfy (== TO)
    _ <- liftHogo $ optional $ satisfy (== NEWLINE)
    name <- extractName
    -- params <- parseParams
    (prog, sub) <- parseProgramMarkers ["REPLACE"]
    put prog
    updateProcedure name ["REPLACE"] sub

-- NOT START POINT \/

parseHogoCodeWithParams :: HogoParser ()
parseHogoCodeWithParams = do
    parseProcedureDeclaration

    let endCheck = True
    unless endCheck parseHogoCode

parseHogoWithParams :: [String] -> HogoParser HogoProgram
parseHogoWithParams params = do
    mapM_ (\(param, value) -> updateVariable param (Variable (Value value))) (zip params [1..])
    parseHogoCodeWithParams
    get

-- | Parsers

parseHogoCode :: HogoParser ()
parseHogoCode = do
    parseProcedureDeclaration

    end <- liftHogo atEnd
    unless end parseHogoCode

parseHogo :: HogoParser HogoProgram
parseHogo = do
    parseHogoCode
    get


