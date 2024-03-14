module Hurtle.Types where

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverlappingInstances #-}
import Text.Megaparsec
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict
import Control.Monad.Except
import Data.Void
import Control.Applicative
import Control.Exception (SomeException)
import Data.Set(Set)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..), toList, head)


--------------------------------------------------------------------------------
-- | Type Definitions
data KeyValue k v 
  -- | Used to represent a variable name
  = Key k 
  -- | Used to represent a numeric type
  | Value v 
  deriving (Show, Eq)

--------------------------------------------------------------------------------
data TOKENS
  -- | Control flow
  = FOR
  | REPEAT
  -- | Brackets
  | LEFTBRACKET 
  | RIGHTBRACKET
  -- | Markers
  | NEWLINE
  -- | Maths
  | SUM
  | DIFFERENCE
  | MULTIPLY
  -- | Variable Creation & Use
  | MAKE
  | SPEECHMARK
  | COLON
  -- | Function Pointers
  | TO
  | END
  -- | Movement
  | FORWARD
  | BACK
  | LEFT
  | RIGHT
  | HOME
  -- |Pen
  | SETWIDTH
  | SETCOLOR
  | PENUP
  | PENDOWN
  | CLS
  -- | Variables / Functions
  | NAME String -- Check this last
  | VALUE Float -- this 2nd to last
  deriving (Show, Ord)

-- | Required because all 'names' and 'values' are equal in syntax
instance Eq TOKENS where
  FOR == FOR = True
  REPEAT == REPEAT = True
  LEFTBRACKET == LEFTBRACKET = True
  RIGHTBRACKET == RIGHTBRACKET = True
  NEWLINE == NEWLINE = True
  SUM == SUM = True
  DIFFERENCE == DIFFERENCE = True
  MULTIPLY == MULTIPLY = True
  MAKE == MAKE = True
  SPEECHMARK == SPEECHMARK = True
  COLON == COLON = True
  TO == TO = True
  END == END = True
  FORWARD == FORWARD = True
  BACK == BACK = True
  LEFT == LEFT = True
  RIGHT == RIGHT = True
  HOME == HOME = True
  SETWIDTH == SETWIDTH = True
  SETCOLOR == SETCOLOR = True
  PENUP == PENUP = True
  PENDOWN == PENDOWN = True
  CLS == CLS = True
  NAME _ == NAME _ = True
  VALUE _ == VALUE _ = True
  _ == _ = False

-- | This is an alias for the Megaparsec parser type; the "Void" tells it that we don't have any custom error type, and the "string" tells it that we're parsing strings.
type Parser = Parsec Void String

instance {-# OVERLAPPING #-} Show [TOKENS] where
    show :: [TOKENS] -> String
    show ts = '\n': showTokensWithIndex 0 ts ++ " EndOfInput\n"

showTokensWithIndex :: Int -> [TOKENS] -> String
showTokensWithIndex _ [] = ""
showTokensWithIndex idx (t:ts) =
    let tokenStr = show t
        arrow = "  -->  "
        padding = replicate (max 0 (3 - length (show idx))) ' ' -- Padding to align the indices
        newline = if (idx + 1) `mod` 7 == 0 then "\n" else ""
    in padding ++ show idx ++ " : " ++ tokenStr ++ arrow ++ newline ++ showTokensWithIndex (idx + 1) ts

newtype TokenParser a = TokenParser {
    runTokenParser :: StateT [TOKENS] Parser a
} deriving (Functor, Applicative, Monad, MonadState [TOKENS], Alternative)
--------------------------------------------------------------------------------

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
  | Back Variable
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
  | Repeat Int [HogoCode]
  | Function String [Variable]
  deriving (Show,Eq)

-- | Parse error handling
newtype HogoParseError = HogoParseError String deriving (Show, Eq, Ord)

instance ShowErrorComponent HogoParseError where
  showErrorComponent :: HogoParseError -> String
  showErrorComponent (HogoParseError msg) = msg

type ParserT = Parsec HogoParseError [TOKENS]

-- | MTL used to combine a parser state with a HogoProgram state monad.
newtype HogoParser a = HogoParser {
    runHogoParser :: StateT HogoProgram ParserT a
} deriving (Functor, Applicative, Monad, MonadState HogoProgram, Alternative)



-- Format a HogoParseError


formatError :: ParseErrorBundle [TOKENS] HogoParseError -> String
formatError bundle = unlines $
    "\n" : map ("  " ++) (formatErrors $ Data.List.NonEmpty.toList $ bundleErrors bundle)

formatErrors :: [ParseError [TOKENS] HogoParseError] -> [String]
formatErrors = map formatErrorEntry


errMsgToList :: Set (ErrorItem (Token [TOKENS])) -> [ErrorItem (Token [TOKENS])]
errMsgToList = Data.Foldable.toList

errMsgToListFancy :: Set (ErrorFancy HogoParseError) -> [ErrorFancy HogoParseError]
errMsgToListFancy = Data.Foldable.toList


formatErrorEntry :: ParseError [TOKENS] HogoParseError -> String
formatErrorEntry (TrivialError pos (Just (Tokens (x :| _))) errMsg) =
    "Error " ++ message
  where
    errMsgs = errMsgToList errMsg
    tokenFaliures = [stuff | (Label stuff) <- errMsgs]
    items = [xs | (_ :| xs) <- tokenFaliures]
    message = "at item " ++ show pos ++ ", token " ++ show x ++ "\n  Message: " ++ show items

formatErrorEntry (FancyError pos errMsg) =
    "Error " ++ message
  where
    errMsgs = errMsgToListFancy errMsg
    res = [hpe | (ErrorFail hpe) <- errMsgs]
    message = "at item " ++ show (pos-1) ++ "\n  Message: " ++ show res

formatErrorEntry err =
  "Error: " ++ message
  where
    message = show err
