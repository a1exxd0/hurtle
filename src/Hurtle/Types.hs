module Hurtle.Types where

import Text.Megaparsec
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict
import Control.Monad.Except
import Data.Void
import Control.Applicative


--------------------------------------------------------------------------------
-- Type Definitions
data KeyValue k v = Key k | Value v deriving (Show, Eq)

-- | A Hogo program is a list of HogoCode instructions
data HogoProgram = HogoProgram {
  varTable :: Map.Map String (KeyValue String Float),
  procTable :: Map.Map String ([String], HogoProgram),
  code     :: [HogoCode]
  } 
  deriving (Show, Eq)

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
  | PenUp
  | PenDown
  | SetWidth Variable
  | SetColor Variable
  | ClearScreen
  -- | Control Flow
  | For [HogoCode] -- enter instansiates into ns, exit gets rid
  | Function String
  deriving (Show,Eq)


-- | This is an alias for the Megaparsec parser type; the "Void" tells it that we don't have any custom error type, and the "string" tells it that we're parsing strings.
type Parser = Parsec Void String

-- | MTL used to combine a parser state with a HogoProgram state monad.
newtype HogoParser a = HogoParser {
    runHogoParser :: StateT HogoProgram Parser a
} deriving (Functor, Applicative, Monad, MonadState HogoProgram, Alternative)
