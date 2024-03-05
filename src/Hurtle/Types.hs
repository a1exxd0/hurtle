module Hurtle.Types where

import Text.Megaparsec
import Data.Void
import Data.Map

--------------------------------------------------------------------------------
-- Type Definitions

-- | A Hogo program is a list of HogoCode instructions
data HogoProgram = HogoProgram {
  varTable :: Map String (Either Variable Float),
  procTable :: Map String ([String], HogoProgram),
  code     :: [HogoCode]
  } 
  deriving (Show, Read, Eq)

data Variable 
  = Variable (Either String Float)
  | Sum Variable Variable
  | Difference Variable Variable
  | Multiply Variable Variable
  deriving (Show, Read, Eq)

data HogoCode
  -- | Movement Commands
  = Forward Variable
  | Backward Variable
  | GoLeft Variable
  | GoRight Variable
  | Home
  -- | Pen Commands
  | PenUp
  | PenDown
  | SetWidth Variable
  | SetColor Variable
  | ClearScreen
  -- | Control Flow
  | For [HogoCode] -- enter instansiates into ns, exit gets rid
  | Function String
  deriving (Show,Read,Eq)


-- | This is an alias for the Megaparsec parser type; the "Void" tells it that we don't have any custom error type, and the "string" tells it that we're parsing strings.
type Parser = Parsec Void String