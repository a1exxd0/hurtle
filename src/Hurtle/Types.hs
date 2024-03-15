{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverlappingInstances #-}

module Hurtle.Types where

-- | Parsing imports
import Text.Megaparsec
    ( Parsec,
      ErrorFancy(ErrorFail),
      ErrorItem(Label, Tokens),
      ParseError(..),
      ParseErrorBundle(bundleErrors),
      ShowErrorComponent(showErrorComponent),
      Stream(Token) )

-- | Data imports for containers
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict ( MonadState, StateT(StateT) )
import Control.Monad.Except ()
import Data.Void ( Void )
import Control.Applicative ( Alternative )
import Data.Set(Set)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..), toList)


--TOKENIZATION STEP-----------------------------------------------------

-- | The TOKENS type is used to represent the different types of tokens that can be found in a Hogo program.
-- | We don't check syntax here, just isolate tokens for ease of comparison later, looking at keywords.
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
  | DIV
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
  | NAME String
  | VALUE Float
  deriving (Show, Ord)

-- | Required because all 'names' and 'values' are equal in syntax
-- | Specific equality check to ignore String/Float differences in variabes
-- | Automatic deriving won't let us do this
instance Eq TOKENS where
  (==) :: TOKENS -> TOKENS -> Bool
  FOR == FOR                    = True
  REPEAT == REPEAT              = True
  LEFTBRACKET == LEFTBRACKET    = True
  RIGHTBRACKET == RIGHTBRACKET  = True
  NEWLINE == NEWLINE            = True
  SUM == SUM                    = True
  DIFFERENCE == DIFFERENCE      = True
  MULTIPLY == MULTIPLY          = True
  DIV == DIV                    = True
  MAKE == MAKE                  = True
  SPEECHMARK == SPEECHMARK      = True
  COLON == COLON                = True
  TO == TO                      = True
  END == END                    = True
  FORWARD == FORWARD            = True
  BACK == BACK                  = True
  LEFT == LEFT                  = True
  RIGHT == RIGHT                = True
  HOME == HOME                  = True
  SETWIDTH == SETWIDTH          = True
  SETCOLOR == SETCOLOR          = True
  PENUP == PENUP                = True
  PENDOWN == PENDOWN            = True
  CLS == CLS                    = True
  NAME _ == NAME _              = True
  VALUE _ == VALUE _            = True

  -- | In any case where tokens arent the same
  _ == _                        = False

-- | Parser type with void error, parsing a list of characters (string)
type Parser = Parsec Void String

-- | Explicit marking for end of input
instance {-# OVERLAPPING #-} Show [TOKENS] where
    show :: [TOKENS] -> String
    show ts = '\n': showTokensWithIndex 0 ts ++ " EndOfInput\n"

-- | Format tokens in max 7 to a line, with spaced out arrows in between
showTokensWithIndex :: Int -> [TOKENS] -> String
showTokensWithIndex _ [] = ""
showTokensWithIndex idx (t:ts) =
    let tokenStr = show t
        arrow = "  -->  "
        padding = replicate (max 0 (3 - length (show idx))) ' ' -- Padding to align the indices
        newline = if (idx + 1) `mod` 7 == 0 then "\n" else ""
    in padding ++ show idx ++ " : " ++ tokenStr ++ arrow ++ newline ++ showTokensWithIndex (idx + 1) ts

-- | Parser state monad for easy tokenization and code safety
newtype TokenParser a = TokenParser {
    runTokenParser :: StateT [TOKENS] Parser a
} deriving (Functor, Applicative, Monad, MonadState [TOKENS], Alternative)




--SYNTAX ANALYSIS STEP-----------------------------------------------------


-- | A KeyValue is used to represent a variable name and its value
data KeyValue k v 
  -- | Used to represent a variable name
  = Key k 
  -- | Used to represent a numeric type
  | Value v 
  deriving (Show, Eq)



-- | A HogoProgram is used to represent the state of a Hogo program at any given time.
-- | It contains a variable table, a procedure table, and a list of code components.
-- | @param@ varTable: A map of variable names to their values
-- | @param@ procTable: A map of procedure names to their arguments and their code
-- | @param@ code: A list of HogoCode components
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
  | Divide Variable Variable
  deriving (Show, Eq)

data HogoCode
  -- | Movement Commands
  = Forward Variable
  | Back Variable
  | GoLeft Variable
  | GoRight Variable
  | Home
  -- | Pen Commands
  | SetWidth Variable
  | SetColor Variable
  | PenUp
  | PenDown
  | ClearScreen
  -- | Variable Usage
  | MakeVariable String Variable
  -- | Control Flow
  | Repeat Variable [HogoCode]
  | For String Variable Variable Variable [HogoCode]
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
