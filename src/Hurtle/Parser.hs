module Hurtle.Parser where

import Hurtle.Types

-- You'll probably want to refer to https://hackage.haskell.org/package/megaparsec for documentation of the Megaparsec library.
import Text.Megaparsec
import Text.Megaparsec.Char

parseHogo :: Parser HogoProgram
parseHogo = error "Implement me :)"