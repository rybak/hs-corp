import Prelude (IO, id, ($), Float, (+), (-), (*))
import Data.Maybe
import Control.Monad

import System.Environment
import System.Exit

import Graphics.Gloss.Interface.IO.Game as G
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color as Color
import Graphics.Gloss.Data.Point

import Menu
import Splash
import Debug

type World = Point

data GameState = Splash SplashType | Playing World | InMenu Menu

displayMode = InWindow "Haskell Corp." (800, 600) (450, 150)
simulationRate = 1000

initialState :: GameState
initialState = InMenu $ createMenu JustStarted

paintGame :: GameState -> IO Picture
paintGame (InMenu m) = return $ renderMenu m
paintGame _ = return $ pictures [text "Not implemented", debugPic]

handleInputs :: Event -> GameState -> IO GameState
handleInputs _ (InMenu (Selected Quit)) = exitWith ExitSuccess
handleInputs e (InMenu m) = return $ InMenu $ handleMenuInputs e m
handleInputs _ x = return x

simulate :: Float -> GameState -> IO GameState
simulate _ x = return x -- do nothing

main :: IO ()
main = playIO displayMode Color.black simulationRate initialState
    paintGame
    handleInputs
    simulate
