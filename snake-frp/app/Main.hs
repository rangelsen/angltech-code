module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game as Gloss
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

main :: IO ()
main = do
  scenePic <- newIORef blank
  drawScene scenePic

  Gloss.playIO
    (InWindow "Snake" (320, 240) (800, 200))
    white
    30
    ()
    (\() -> readIORef scenePic)
    (\_ () -> pure ())
    (\_ () -> pure ())

drawScene :: IORef Picture -> IO ()
drawScene picRef =
    writeIORef picRef (rectangleSolid 20.0 20.0)
