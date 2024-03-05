module Hurtle.FileReader(
    readFileToLower
) where

import qualified Data.ByteString as BS
import Data.Char ( toLower, ord, chr )
import Data.Word

toLowerExceptNewline :: Word8 -> Char
toLowerExceptNewline w
    | w == fromIntegral (ord '\n') = chr $ fromIntegral w
    | otherwise = chr (ord (toLower (chr (fromIntegral w))))

readFileToLower :: String -> IO String
readFileToLower s = do
    contents <- BS.readFile "test/test.hogo"
    pure $ map toLowerExceptNewline $ BS.unpack contents
