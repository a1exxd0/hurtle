module Hurtle.Types where

import Text.Megaparsec
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict
import Control.Monad.Except
import Data.Void
import Control.Applicative


--------------------------------------------------------------------------------
-- | Type Definitions
data KeyValue k v 
  -- | Used to represent a variable name
  = Key k 
  -- | Used to represent a numeric type
  | Value v 
  deriving (Show, Eq)

-- | A Hogo program is a list of HogoCode instructions
data HogoProgram = HogoProgram {
  varTable :: Map.Map String Variable,
  procTable :: Map.Map String ([String], HogoProgram),
  code     :: [HogoCode]
  } 
  deriving (Eq)


instance Show HogoProgram where
  show :: HogoProgram -> String
  show hogo = 
    "\n" ++ "Variable Table: " ++ "\n" 
    ++ concatMap (\e -> "   " ++ show e ++ "\n") (Map.toList $ varTable hogo) ++ "\n" ++
    "Procedure Table: " ++ "\n"
    ++ concatMap (\(e, f) -> "   " ++ show e ++ ":   " ++ show f ++ "\n") (Map.toList $ procTable hogo) ++ "\n" ++
    "Code Components: " ++ "\n" ++ concatMap (\e -> "   " ++ show e ++ "\n") (code hogo)

data Variable 
  = Variable (KeyValue String Float)
  | Sum Variable Variable
  | Difference Variable Variable
  | Multiply Variable Variable
  deriving (Show, Eq)

data HogoCode
  -- | Movement Commands
  = Forward Variable
  | Backward Variable
  | GoLeft Variable
  | GoRight Variable
  | Home
  -- | Variable Usage
  | MakeVariable String Variable
  -- | Pen Commands
  | SetWidth Variable
  | SetColor Variable
  | PenUp
  | PenDown
  | ClearScreen
  -- | Control Flow
  | For String Variable Variable Variable [HogoCode] -- enter instansiates into ns, exit gets rid
  | Function String [Variable]
  | Scope HogoProgram
  deriving (Show,Eq)


-- | This is an alias for the Megaparsec parser type; the "Void" tells it that we don't have any custom error type, and the "string" tells it that we're parsing strings.
type Parser = Parsec Void String

-- | MTL used to combine a parser state with a HogoProgram state monad.
newtype HogoParser a = HogoParser {
    runHogoParser :: StateT HogoProgram Parser a
} deriving (Functor, Applicative, Monad, MonadState HogoProgram, Alternative)
