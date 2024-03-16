module Hurtle.FileReader(
    readFileToLower
) where

import qualified Data.ByteString as BS
import Data.Char ( toLower, ord, chr )
import Data.Word

-- | This function takes a Word8 and converts it to a Char, but only
--   if it is not a newline character, since @'\n' -> "\n"@, which is
--   no longer a character.
toLowerExceptNewline 
    :: Word8        -- ^ 8 bit character
    -> Char         -- ^ 16 bit character
toLowerExceptNewline w
    | w == fromIntegral (ord '\n')      = chr $ fromIntegral w
    | otherwise                         = chr (ord (toLower (chr (fromIntegral w))))

-- | This function reads a file and converts it to lowercase, except for newline characters.
readFileToLower 
    :: String           -- ^ File path
    -> IO String        -- ^ File contents in lowercase wrapped in IO
readFileToLower s = do
    contents <- BS.readFile s
    pure $ map toLowerExceptNewline $ BS.unpack contents