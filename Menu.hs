module Menu where

import qualified Graphics.Gloss.Interface.IO.Game as G
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color as Color

import Debug

data Item = Continue | NewGame | LoadGame | SaveGame | Quit

instance Show Item where
    show Continue = "Continue"
    show NewGame = "New game"
    show LoadGame = "Load game"
    show SaveGame = "Save game"
    show Quit = "Quit"

data Menu = Presenting [Item] Int | Selected Item

data MenuType = JustStarted | Paused

createMenu :: MenuType -> Menu
createMenu JustStarted = Presenting [NewGame, LoadGame, Quit] 0
createMenu Paused = Presenting [Continue, SaveGame, Quit] 0

menuTextScale :: Float
menuTextScale = 0.25
menuSize = menuTextScale * 200.0

data MenuAction = Up | Down | Select | Idle

handleMenuInputs :: G.Event -> Menu -> Menu
handleMenuInputs (G.EventKey k G.Down _ _) m@(Presenting xs i) =
    case (parseMenuKey k) of
        Idle   -> m
        Up     -> Presenting xs (i - 1)
        Down   -> Presenting xs (i + 1)
        Select -> Selected (xs !! (i `mod` (length xs)))
handleMenuInputs _ m = m

parseMenuKey :: G.Key -> MenuAction
parseMenuKey (G.Char 'w') = Up
parseMenuKey (G.Char 's') = Down
parseMenuKey (G.SpecialKey G.KeyEnter) = Select
parseMenuKey (G.SpecialKey G.KeySpace) = Select
parseMenuKey _ = Idle

renderMenu :: Menu -> Picture
renderMenu (Presenting xs i) = renderPresenting xs i
renderMenu (Selected x) = pictures [renderMenuItem x, debugPic]

renderPresenting :: [Item] -> Int -> Picture
renderPresenting xs i = pictures $ (renderMarker i n) : (map render indexed) where
    n :: Int
    n = length xs
    indexed = zip xs [0..]
    render :: (Item, Int) -> Picture
    render (m, y) = translate (-2.0 * menuSize) (calcY y n) $ renderMenuItem m

renderMenuItem :: Item -> Picture
renderMenuItem = applyStyle . resize . text . show where
    applyStyle :: Picture -> Picture
    applyStyle = color Color.green
    resize :: Picture -> Picture
    resize = scale menuTextScale menuTextScale

renderMarker :: Int -> Int -> Picture
renderMarker i n = applyStyle $ position marker where
    applyStyle :: Picture -> Picture
    applyStyle = color Color.yellow
    position :: Picture -> Picture
    position = translate (-2.5 * menuSize) (calcY i n + menuSize * 0.3)

calcY :: Int -> Int -> Float
calcY i n = (fromIntegral (n `div` 2 - i `mod` n) + 1.5) * menuSize

marker :: Picture
marker = Polygon [(-20.0, -10.0), (0.0, 0.0), (-20.0, 10.0)]


