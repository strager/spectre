module Main where

import Control.Concurrent.Chan
import Control.Concurrent
import Control.Monad
import Data.Function

import qualified Graphics.UI.SDL as SDL

data Screen = Screen
  { screenWidth :: Int
  , screenHeight :: Int
  , screenSurface :: SDL.Surface
  }

data LoopControl = Continue | Break

main :: IO ()
main = SDL.withInit [SDL.InitEverything] $ do
  surface <- SDL.setVideoMode initialWidth initialHeight 32
    [SDL.HWSurface, SDL.Resizable, SDL.RLEAccel, SDL.DoubleBuf]
  let initialScreen = Screen initialWidth initialHeight surface

  mainLoop initialScreen

  where
    initialWidth = 640
    initialHeight = 480

    mainLoop screen = pollEvent
      where
        pollEvent = SDL.pollEvent >>= handleEvent

        handleEvent event = do
          control <- step screen event
          case control of
            Break -> return ()
            Continue -> case event of
              SDL.NoEvent
                -> SDL.waitEvent >>= handleEvent
              _ -> pollEvent

step screen event = case event of
  SDL.KeyDown (SDL.Keysym key _ _)
    -> if key == SDL.SDLK_ESCAPE
         then return Break
         else return Continue
  SDL.Quit -> return Break
  SDL.NoEvent -> redraw screen >> return Continue
  _ -> return Continue

redraw screen = do
  let surface = screenSurface screen
  let format = SDL.surfaceGetPixelFormat surface
  color <- SDL.mapRGB format 0 128 255
  void $ SDL.fillRect surface Nothing color
  SDL.flip surface
