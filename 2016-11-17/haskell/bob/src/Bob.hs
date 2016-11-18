module Bob (responseFor) where

import Data.Char (isAlphaNum, isLetter, isSpace, isUpper)
import Data.Text (pack, strip)
import qualified Data.Text as T (last)

responseFor :: String -> String
responseFor s
  | all isSpace s = "Fine. Be that way!"
  | myAll isUpper $ filter isLetter s = "Whoa, chill out!"
  | T.last (strip $ pack s) == '?' = "Sure."
  | otherwise = "Whatever."

myAll :: (a -> Bool) -> [a] -> Bool
myAll _ [] = False
myAll f l = all f l
