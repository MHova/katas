module Robot
    ( Bearing(East,North,South,West)
    , Robot
    , bearing
    , coordinates
    , mkRobot
    , simulate
    , turnLeft
    , turnRight
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show)

type Coordinates = (Integer, Integer)

data Robot = Robot 
  { rBearing :: Bearing
  , rCoord :: Coordinates
  }

bearing :: Robot -> Bearing
bearing = rBearing

coordinates :: Robot -> (Integer, Integer)
coordinates = rCoord

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot

simulate :: Robot -> String -> Robot
simulate = undefined

turnLeft :: Bearing -> Bearing
turnLeft North = West
turnLeft East = North
turnLeft South = East
turnLeft West = South

turnRight :: Bearing -> Bearing
turnRight = turnLeft . turnLeft . turnLeft
