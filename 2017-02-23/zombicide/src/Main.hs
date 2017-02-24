module Main where

type Equipment = String

data Status = Dead | Alive Int

data Survivor =
  Survivor
  { name :: String
  , status :: Status
  , actions :: Int
  , inHand :: [Equipment]
  , reserve :: [Equipment]
  }

initialSurvivor :: String -> Survivor
initialSurvivor n = Survivor n (Alive 0) 3 [] []

woundOnce :: Survivor -> Survivor
woundOnce s@(Survivor _ Dead _ _ _) = s
woundOnce s@(Survivor _ (Alive wounds) _ _ _)
  | wounds + 1 >= 2 = s{status = Dead}
  | otherwise = s{status = Alive (wounds + 1)}

wound :: Survivor -> Int -> Survivor
wound s wounds = foldl (\b a -> a b) s (replicate wounds woundOnce)

main :: IO ()
main = putStrLn "hello world"
