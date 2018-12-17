module Gam.Main.Tea where

import Gam.Internal.Prelude
import Gam.Internal.Window  (Window)

import qualified Gam.Internal.Render as Render
import qualified Gam.Internal.Window as Window

import Control.Concurrent (threadDelay)
import GHC.Clock

import qualified SDL


type Event
  = SDL.EventPayload

main ::
     state
  -> (Int -> state -> state)
  -> (Event -> state -> state)
  -> (state -> Window)
  -> IO ()
main state step handle render = do
  SDL.initialize [SDL.InitVideo]

  window <-
    Window.new (render state)

  renderer <-
    SDL.createRenderer window (-1) SDL.RendererConfig
      { SDL.rendererType = SDL.AcceleratedVSyncRenderer
      , SDL.rendererTargetTexture = False
      }

  now <- monotonicMicros

  loop
    step
    handle
    (Render.run window renderer . Window.render . render)
    now
    state

loop
  :: forall state.
     (Int -> state -> state)
  -> (Event -> state -> state)
  -> (state -> IO ())
  -> Int
  -> state
  -> IO ()
loop step handle render =
  go
  where
    go :: Int -> state -> IO ()
    go prev state0 = do
      time0 <- monotonicMicros

      let
        handleAll state =
          SDL.pollEvent >>= \case
            Nothing -> pure state
            Just event -> handleAll (handle (SDL.eventPayload event) state)

      state1 <-
        step (time0 - prev) <$> handleAll state0

      render state1

      time1 <- monotonicMicros

      threadDelay (time0 + microsPerFrame - time1)

      go time0 state1

monotonicMicros :: IO Int
monotonicMicros = do
  ns <- getMonotonicTimeNSec
  pure (fromIntegral (ns `div` 1000))

-- 60 fps
microsPerFrame :: Int
microsPerFrame =
  16667
