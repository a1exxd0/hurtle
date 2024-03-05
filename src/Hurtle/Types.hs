module Hurtle.Types where

import Text.Megaparsec
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict
import Control.Monad.Except
import Data.Void
import Control.Applicative


--------------------------------------------------------------------------------
-- Type Definitions

-- | A Hogo program is a list of HogoCode instructions
data HogoProgram = HogoProgram {
  varTable :: Map.Map String (Either Variable Float),
  procTable :: Map.Map String ([String], HogoProgram),
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
  -- | Variable Usage
  | MakeVariable String Variable
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

-- | MTL usage to bring state with program

newtype HogoParser a = HogoParser {
    runHogoParser :: StateT HogoProgram Parser a
} deriving (Functor, Applicative, Monad, MonadState HogoProgram, Alternative)
