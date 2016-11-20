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
turnLeft = undefined

turnRight :: Bearing -> Bearing
turnRight = undefined
