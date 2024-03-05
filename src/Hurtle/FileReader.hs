module Hurtle.FileReader(
    readFileToLower
) where

import qualified Data.ByteString as BS
import Data.Char ( toLower )

readFileToLower :: String -> IO String
readFileToLower s = do
    contents <- BS.readFile s
    pure $ map toLower $ show contents