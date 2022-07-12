module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game as Gloss hiding (Up)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Tuple.Strict

type Coord = (Int, Int)
type BoardPos = Coord
type WindowPos = (Float, Float)
type Size = (Int, Int)
type BoardSize = Size
type WindowSize = Size
type Snake = [BoardPos]
data Direction = Up | Down | Left | Right deriving (Show, Eq)
data GameState = GameState
    { snake :: Snake
    , direction :: Direction
    }

boardSize = (10, 10) :: BoardSize
windowSize = (320, 320) :: WindowSize
cellSize = pairToFloat $ combinePair div windowSize boardSize

combinePair :: (a -> a -> b) -> (a, a) -> (a, a) -> (b, b)
combinePair f (a1, a2) (a3, a4) = (f a1 a3, f a2 a4)

applyPair :: (a -> b) -> (a, a) -> (b, b)
applyPair f (a1, a2) = (f a1, f a2)

pairToFloat = applyPair fromIntegral

drawScene :: GameState -> IORef Picture -> IO ()
drawScene state picRef = do
    pic <- readIORef picRef
    let newPic = drawSnake (snake state) pic
    writeIORef picRef newPic

drawSnake :: Snake -> Picture -> Picture
drawSnake [] pic = pic
drawSnake (coord:coords) pic =
    let (w, h) = cellSize
        (x, y) = boardPosToWindow coord
        rect = rectangleSolid w h
        newPic = pictures
            [ translate x y rect
            , pic
            ]
    in drawSnake coords newPic

boardPosToWindow :: BoardPos -> WindowPos
boardPosToWindow boardPos =
    let (x, y) = combinePair (*) cellSize (pairToFloat boardPos)
        (winW, winH) = pairToFloat windowSize
        (cellW, cellH) = cellSize
    in (x - winW/2 + cellW/2 , y - winH/2 + cellH/2)

main :: IO ()
main = do
  scenePic <- newIORef blank
  drawScene (GameState [(2, 2), (2, 3), (2, 1), (1, 1)] Up) scenePic

  Gloss.playIO
    (InWindow "Snake" windowSize (800, 200))
    white
    30
    ()
    (\() -> readIORef scenePic)
    (\_ () -> pure ())
    (\_ () -> pure ())

