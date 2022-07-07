module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game as Gloss
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Tuple.Strict

type Coord = (Int, Int)
type BoardPos = Coord
type WindowPos = (Float, Float)
type Size = (Int, Int)
type BoardSize = Size
type WindowSize = Size
type Snake = [Coord]
data GameState = GameState
    { snake :: Snake
    }

boardSize = (10, 10) :: BoardSize
windowSize = (320, 320) :: WindowSize
cellSize = pairToFloat $ combinePair div windowSize boardSize

combinePair :: (a -> a -> b) -> (a, a) -> (a, a) -> (b, b)
combinePair f (a1, a2) (a3, a4) = (f a1 a3, f a2 a4)

applyPair :: (a -> b) -> (a, a) -> (b, b)
applyPair f (a1, a2) = (f a1, f a2)

pairToFloat coord = applyPair fromIntegral coord

drawScene :: GameState -> IORef Picture -> IO ()
drawScene state picRef = do
    drawSnake (snake state) picRef

drawSnake :: Snake -> IORef Picture -> IO ()
drawSnake snake picRef =
    let (w, h) = cellSize
        (x, y) = boardPosToWindow (9, 9)
        rect = rectangleSolid w h
    in writeIORef picRef (translate x y rect)

boardPosToWindow :: BoardPos -> WindowPos
boardPosToWindow boardPos =
    let (x, y) = combinePair (*) cellSize (pairToFloat boardPos)
        (winW, winH) = pairToFloat windowSize
        (cellW, cellH) = cellSize
    in (x - winW/2 + cellW/2 , y - winH/2 + cellH/2)

main :: IO ()
main = do
  scenePic <- newIORef blank
  drawScene (GameState [(1,0), (0, 0)]) scenePic

  Gloss.playIO
    (InWindow "Snake" windowSize (800, 200))
    white
    30
    ()
    (\() -> readIORef scenePic)
    (\_ () -> pure ())
    (\_ () -> pure ())

