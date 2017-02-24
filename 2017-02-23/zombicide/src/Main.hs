{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Maybe(isJust)


type Equipment = String

data DeadSurvivor = DeadSurvivor

data LivingSurvivor = LivingSurvivor
  { name :: String
  , wounds :: Int
  , actions :: Int
  , leftHand :: Maybe Equipment
  , rightHand :: Maybe Equipment
  , reserve :: [Equipment]
  , capacity :: Int
  }

data Survivor = Dead DeadSurvivor | Living LivingSurvivor

corpse :: Survivor
corpse = Dead DeadSurvivor

initialSurvivor :: String -> Survivor
initialSurvivor n = Living $ LivingSurvivor n 0 3 Nothing Nothing [] 5

pickup :: Equipment -> LivingSurvivor -> LivingSurvivor
pickup e survivor = if canHoldMore survivor then equip survivor else survivor
  where
    equip s@LivingSurvivor{leftHand = Nothing}= s{leftHand = Just e}
    equip s@LivingSurvivor{rightHand = Nothing}= s{rightHand = Just e}
    equip s@LivingSurvivor{reserve}= s{reserve = e:reserve}

canHoldMore :: LivingSurvivor -> Bool
canHoldMore s@LivingSurvivor{capacity} = numEquip s < capacity

needsToDrop :: LivingSurvivor -> Bool
needsToDrop s@LivingSurvivor{capacity} = numEquip s > capacity

numEquip :: LivingSurvivor -> Int
numEquip LivingSurvivor{leftHand, rightHand, reserve} = length reserve + length (filter isJust [leftHand, rightHand])

dropEquipment :: LivingSurvivor -> LivingSurvivor
dropEquipment s@LivingSurvivor{reserve = _:es} = s{reserve = es}
dropEquipment s@LivingSurvivor{leftHand = Just _} = s{leftHand = Nothing}
dropEquipment s@LivingSurvivor{rightHand = Just _} = s{rightHand = Nothing}
dropEquipment s = s

woundOnce :: Survivor -> Survivor
woundOnce (Dead _) = corpse
woundOnce (Living ls@LivingSurvivor{wounds, capacity})
  | newWounds >= 2 = corpse
  | otherwise =
    let wounded = ls{wounds = newWounds, capacity = newCapacity}
    in if needsToDrop wounded then Living (dropEquipment wounded) else Living wounded
  where
    newWounds = wounds + 1
    newCapacity = capacity - 1

wound :: LivingSurvivor -> Int -> Survivor
wound s numWounds = foldl (\b a -> a b) (Living s) (replicate numWounds woundOnce)

main :: IO ()
main = putStrLn "hello world"
