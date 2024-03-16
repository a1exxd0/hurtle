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
--
--   We don't check syntax here, just isolate tokens for ease of comparison later, looking at keywords.
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
  -- | Pen commands
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
--
--   Specific equality check to ignore String/Float differences in variabes,
--   since utomatic deriving won't let us do this.
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
  _ == _                        = False -- ^ In any case where tokens arent the same

-- | Parser type with void error, parsing a list of characters (string)
type Parser = Parsec Void String

-- | Explicit marking for end of input
instance {-# OVERLAPPING #-} Show [TOKENS] where
    show :: [TOKENS] -> String
    show ts = '\n': showTokensWithIndex 0 ts ++ " EndOfInput\n"

-- | Format tokens in max 7 to a line, with spaced out arrows in between
showTokensWithIndex 
  :: Int          -- ^ Index of token
  -> [TOKENS]     -- ^ List of tokens
  -> String       -- ^ String representation of tokens
showTokensWithIndex _ [] = ""
showTokensWithIndex idx (t:ts) =
    let tokenStr = show t
        arrow = "  -->  "
        padding = replicate (max 0 (3 - length (show idx))) ' '
        newline = if (idx + 1) `mod` 7 == 0 then "\n" else ""
    in padding ++ show idx ++ " : " ++ tokenStr ++ arrow ++ newline ++ showTokensWithIndex (idx + 1) ts

-- | Parser state monad for easy tokenization and code safety
newtype TokenParser a = TokenParser {
    runTokenParser :: StateT [TOKENS] Parser a
} deriving (Functor, Applicative, Monad, MonadState [TOKENS], Alternative)




--SYNTAX ANALYSIS STEP-----------------------------------------------------


-- | A KeyValue is used to represent a variable name and its value
data KeyValue k v 
  = Key k           -- ^ Used to represent a variable name
  | Value v         -- ^ Used to represent a numeric type (or value of sorts)
  deriving (Show, Eq)



-- | A HogoProgram is used to represent the state of a Hogo program at any given time.
--   It contains a variable table, a procedure table, and a list of code components.
--   @param@ varTable: A map of variable names to their values
--   @param@ procTable: A map of procedure names to their arguments and their code
--   @param@ code: A list of HogoCode components
data HogoProgram = HogoProgram {
  varTable :: Map.Map String Variable,                    -- ^ Variable Table
  procTable :: Map.Map String ([String], HogoProgram),    -- ^ Procedure Table
  code     :: [HogoCode]                                  -- ^ Code Components
  } 
  deriving (Eq)


{-
  A mini commentary on my usage of strict maps!

  1) More performance efficient generally
  2) Guaranteed predictability when I perform an action, i.e.
      I know that if I add a variable to the table, it will be there
      and I can access it immediately.
-}


-- | Show instance for HogoProgram to format nicely.
--   Subprograms are NOT inlined (for procedures). 
--   This is for debugging purposes but can suffice.
instance Show HogoProgram where
  show :: HogoProgram -> String
  show hogo = 
    "\n" ++ "Variable Table: " ++ "\n" 
    ++ concatMap (\e -> "   " ++ show e ++ "\n") (Map.toList $ varTable hogo) ++ "\n" ++
    "Procedure Table: " ++ "\n"
    ++ concatMap (\(e, f) -> "   " ++ show e ++ ":   " ++ show f ++ "\n") (Map.toList $ procTable hogo) ++ "\n" ++
    "Code Components: " ++ "\n" ++ concatMap (\e -> "   " ++ show e ++ "\n") (code hogo)

-- | A 'Variable' is used to represent a float or variable in a Hogo program.
data Variable 
  = Variable (KeyValue String Float) -- ^ Either a string name or a float value
  | Sum Variable Variable            -- ^ Sum of two variables
  | Difference Variable Variable     -- ^ Difference of two variables
  | Multiply Variable Variable       -- ^ Product of two variables
  | Divide Variable Variable         -- ^ Quotient of two variables
  deriving (Show, Eq)

-- | A 'HogoCode' is used to represent the different types of commands that can be found in a Hogo program.
data HogoCode

  -- | Movement Commands
  = Forward     -- ^ Move forward
    Variable
  | Back        -- ^ Move back
    Variable
  | GoLeft      -- ^ Turn left
    Variable              -- ^ By a variable angle in degrees
  | GoRight     -- ^ Turn right  
    Variable              -- ^ By a variable angle in degrees
  | Home        -- ^ Return to the origin


  -- | Pen Commands

  | SetWidth    -- ^ Set the width of the pen
    Variable              -- ^ To a variable width
  | SetColor    -- ^ Set the color of the pen 
    Variable              -- ^ To a variable (floored in the code and taken modulus to fit in range)
  | PenUp       -- ^ Stop drawing
  | PenDown     -- ^ Start drawing (initial state)
  | ClearScreen -- ^ Clear the screen


  -- | Variable Usage

  | MakeVariable -- ^ Create a variable
    String                -- ^ With a string name
    Variable              -- ^ And a variable value


  -- | Control Flow

  | Repeat      -- ^ Repeat a block of code
    Variable              -- ^ A variable (floored) number of times
    [HogoCode]            -- ^ A list of code to repeat
  | For         -- ^ For loop
    String                -- ^ A string name
    Variable              -- ^ Starting value
    Variable              -- ^ Ending value 
    Variable              -- ^ Increment value
    [HogoCode]            -- ^ A list of code to repeat
  | Function    -- ^ Function definition
    String                -- ^ A string name
    [Variable]            -- ^ A list of input values

  deriving (Show,Eq)

-- | Parse error handling
newtype HogoParseError = HogoParseError String deriving (Show, Eq, Ord)

-- | Show instance for HogoParseError
instance ShowErrorComponent HogoParseError where
  showErrorComponent :: HogoParseError -> String
  showErrorComponent (HogoParseError msg) = msg


-- | Parser type with HogoParseError error, parsing a list of TOKENS (and not characters)
type ParserT = Parsec HogoParseError [TOKENS]

-- | MTL used to combine a parser state with a HogoProgram state monad.
newtype HogoParser a = HogoParser {
    runHogoParser :: StateT HogoProgram ParserT a
} deriving (Functor, Applicative, Monad, MonadState HogoProgram, Alternative)



-- | Format error messages for the parser
formatError 
  :: ParseErrorBundle [TOKENS] HogoParseError -- ^ Error bundle
  -> String
formatError bundle = unlines $
    "\n" : map ("  " ++) (formatErrors $ Data.List.NonEmpty.toList $ bundleErrors bundle)

-- | Format multiple error messages for the parser
formatErrors 
  :: [ParseError [TOKENS] HogoParseError]     -- ^ List of error bundles
  -> [String]
formatErrors = map formatErrorEntry

-- | Convert a set of error items to a list
errMsgToList 
  :: Set (ErrorItem (Token [TOKENS]))  -- ^ Set of error items
  -> [ErrorItem (Token [TOKENS])]
errMsgToList = Data.Foldable.toList

-- | Convert a set of error fancy items to a list
errMsgToListFancy 
  :: Set (ErrorFancy HogoParseError)  -- ^ Set of error fancy items
  -> [ErrorFancy HogoParseError]
errMsgToListFancy = Data.Foldable.toList


-- | Format a single error message for the parser in a nice format
formatErrorEntry 
  :: ParseError [TOKENS] HogoParseError -- ^ Error bundle
  -> String

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

formatErrorEntry err = "Error: " ++ show err
