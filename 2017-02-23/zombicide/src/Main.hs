{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Maybe(isJust)


type Equipment = String

data Status = Dead | Alive Int

data Survivor =
  Survivor
  { name :: String
  , status :: Status
  , actions :: Int
  , leftHand :: Maybe Equipment
  , rightHand :: Maybe Equipment
  , reserve :: [Equipment]
  , capacity :: Int
  }

initialSurvivor :: String -> Survivor
initialSurvivor n = Survivor n (Alive 0) 3 Nothing Nothing [] 5

pickup :: Equipment -> Survivor -> Survivor
pickup e survivor = if canHoldMore survivor then equip survivor else survivor
  where
    equip s@Survivor{leftHand = Nothing}= s{leftHand = Just e}
    equip s@Survivor{rightHand = Nothing}= s{rightHand = Just e}
    equip s@Survivor{reserve}= s{reserve = e:reserve}

canHoldMore :: Survivor -> Bool
canHoldMore s@Survivor{capacity} = numEquip s < capacity

needsToDrop :: Survivor -> Bool
needsToDrop s@Survivor{capacity} = numEquip s > capacity

numEquip :: Survivor -> Int
numEquip Survivor{leftHand, rightHand, reserve} = length reserve + length (filter isJust [leftHand, rightHand])

dropEquipment :: Survivor -> Survivor
dropEquipment s@Survivor{reserve = _:es} = s{reserve = es}
dropEquipment s@Survivor{leftHand = Just _} = s{leftHand = Nothing}
dropEquipment s@Survivor{rightHand = Just _} = s{rightHand = Nothing}
dropEquipment s = s

woundOnce :: Survivor -> Survivor
woundOnce s@Survivor{status = Dead} = s
woundOnce s@Survivor{status = Alive wounds, capacity}
  | newWounds >= 2 = s{status = Dead}
  | otherwise =
    let res = s{status = Alive newWounds, capacity = newCapacity}
    in if needsToDrop res then dropEquipment res else res
  where
    newWounds = wounds + 1
    newCapacity = capacity - 1

wound :: Survivor -> Int -> Survivor
wound s numWounds = foldl (\b a -> a b) s (replicate numWounds woundOnce)

main :: IO ()
main = putStrLn "hello world"
