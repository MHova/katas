{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Maybe(isJust)

newtype RunningGame = RunningGame
  { survivors :: [Survivor] }
data EndedGame = EndedGame
data Game = Running RunningGame | Ended EndedGame

newGame :: Game
newGame = Running $ RunningGame []

addSurvivor :: String -> RunningGame -> RunningGame
addSurvivor name g@RunningGame{survivors}
  | name `elem` map getName survivors = g
  | otherwise = g{survivors = newSurvivor name:survivors}

type Equipment = String

newtype DeadSurvivor = DeadSurvivor
  { dname :: String}
data LivingSurvivor = LivingSurvivor
  { lname :: String
  , wounds :: Int
  , actions :: Int
  , leftHand :: Maybe Equipment
  , rightHand :: Maybe Equipment
  , reserve :: [Equipment]
  , capacity :: Int
  }
data Survivor = Dead DeadSurvivor | Living LivingSurvivor

getName :: Survivor -> String
getName (Dead DeadSurvivor{dname}) = dname
getName (Living LivingSurvivor{lname}) = lname

corpse :: String -> Survivor
corpse name = Dead $ DeadSurvivor name

newSurvivor :: String -> Survivor
newSurvivor n = Living $ LivingSurvivor n 0 3 Nothing Nothing [] 5

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
woundOnce dead@(Dead _) = dead
woundOnce (Living ls@LivingSurvivor{lname, wounds, capacity})
  | newWounds >= 2 = corpse lname
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
