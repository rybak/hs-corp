module Main where

import Prelude (IO, id, ($), Float, (+), (-), (*))
import Data.Maybe
import Control.Monad
import System.Environment
import System.Exit

import Graphics.Gloss.Interface.IO.Game as G
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color as Color
import Graphics.Gloss.Data.Point

data Direction = Up | Down | Right | Left

-- World consists of
--  Game world
--   Just a point for now
--  State of inputs
--   Just the direction of movement for now
data World = World Point (Maybe Direction)

displayMode = InWindow "Haskell Corp." (800, 600) (10, 10)
simulationRate = 1000

initialWorld = World (0.0, 0.0) Nothing

paintWorld :: World -> IO Picture
paintWorld (World (x, y) _) = return $ Translate x y $
    Color Color.green $ circle 50.0


-- handleInputs only changes the state of inputs
handleInputs :: Event -> World -> IO World
-- key down => choose movement direction
handleInputs (EventKey key G.Down _ _) (World pos dir) = return $ World pos (parse key)
-- key unpressed => remove movement
handleInputs (EventKey key G.Up _ _) (World pos dir) = return $ World pos Nothing
handleInputs _ w = return w

parse :: Key -> Maybe Direction
parse (Char c) = case c of
    'w' -> Just Main.Up
    'a' -> Just Main.Left
    's' -> Just Main.Down
    'd' -> Just Main.Right
    _   -> Nothing
parse _ = Nothing


-- simulate only changes the state of game
-- moves the point in the direction with some speed
simulate :: Float -> World -> IO World
simulate t (World pos dir) = return $ World (move dir t pos) dir

move :: Maybe Direction -> Float -> Point -> Point
move Nothing _ p = p
move (Just dir) rate (x, y) = case dir of
    Main.Up    -> (x, y + speed)
    Main.Down  -> (x, y - speed)
    Main.Right -> (x + speed, y)
    Main.Left  -> (x - speed, y)
    where speed = 50.0 * rate

main :: IO ()
-- :: Display -- Display mode.
-- -> Color  --Background color.
-- -> Int -- Number of simulation steps to take for each second of real time.
-- -> world -- The initial world.
main = playIO displayMode Color.black simulationRate initialWorld
    -- -> (world -> IO Picture) -- An action to convert the world a picture.
    paintWorld
    -- -> (Event -> world -> IO world) -- A function to handle input events.
    handleInputs
    -- -> (Float -> world -> IO world) -- A function to step the world one iteration. It is passed the period of time (in seconds) needing to be advanced.
    simulate
    -- -> IO ()

