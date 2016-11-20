{-# LANGUAGE NamedFieldPuns #-}

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

coordinates :: Robot -> Coordinates
coordinates = rCoord

mkRobot :: Bearing -> Coordinates -> Robot
mkRobot = Robot

simulate :: Robot -> String -> Robot
simulate = foldl parse
  where
    parse r 'R' = goRight r
    parse r 'L' = goLeft r
    parse r 'A' = advance r
    parse _ x = error [x]
    goLeft r@Robot{rBearing} = r{rBearing = turnLeft rBearing}
    goRight r@Robot{rBearing} = r{rBearing = turnRight rBearing}

turnLeft :: Bearing -> Bearing
turnLeft North = West
turnLeft East = North
turnLeft South = East
turnLeft West = South

turnRight :: Bearing -> Bearing
turnRight = turnLeft . turnLeft . turnLeft

advance :: Robot -> Robot
advance r@(Robot North (x,y)) = r{rCoord = (x, y+1)}
advance r@(Robot East (x,y)) = r{rCoord = (x+1, y)}
advance r@(Robot South (x,y)) = r{rCoord = (x, y-1)}
advance r@(Robot West (x,y)) = r{rCoord = (x-1, y)}

