module Math where

import Data.Maybe (fromMaybe)
import Safe (headMay)

add' :: Int -> Int -> Int
add' a b = foldr ($) 0 $ replicate a (+ 1) ++ replicate b (+ 1)

subtract' :: Int -> Int -> Int
subtract' a b = foldr ($) 0 $ replicate a (+ 1) ++ replicate b (+ (-1))

multiply' :: Int -> Int -> Int
multiply' a b = fromMaybe 0 (headMay . reverse . take a $ iterate (add' b) b)

divide' :: Int -> Int -> Int
divide' a b = subtract' (length . takeWhile (>= 0) $ iterate (`subtract'` b) a) 1

data LinkedList =
  Cons Int LinkedList |
  End
    deriving (Show)

addLinks :: LinkedList -> LinkedList -> LinkedList
addLinks list1 list2 = undefined
