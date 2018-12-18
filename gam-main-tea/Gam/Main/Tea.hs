module Gam.Main.Tea where

import Gam.Internal.Prelude
import Gam.Internal.Window  (Window)
import Internal.Sub         (Sub(..))

import qualified Gam.Internal.Render           as Render
import qualified Gam.Internal.SpriteSheetCache as SpriteSheetCache
import qualified Gam.Internal.Window           as Window

import Control.Concurrent (threadDelay)
import GHC.Clock
import System.Mem         (performGC)

import qualified SDL


main ::
     state
  -> Sub msg
  -> (msg -> state -> state)
  -> (state -> Window)
  -> IO ()
main state (Sub subFps subSdl sdlInits) update render = do
  SDL.initialize (SDL.InitVideo : sdlInits)

  window <-
    Window.new (render state)

  renderer <-
    SDL.createRenderer window (-1) SDL.RendererConfig
      { SDL.rendererType = SDL.AcceleratedVSyncRenderer
      , SDL.rendererTargetTexture = False
      }

  spriteSheetCache <-
    SpriteSheetCache.new

  loop
    subFps
    subSdl
    update
    (Render.run window renderer spriteSheetCache . Window.render . render)
    state

loop
  :: forall msg state.
     Maybe (Int -> msg)
  -> Maybe (SDL.EventPayload -> Maybe msg)
  -> (msg -> state -> state)
  -> (state -> IO ())
  -> state
  -> IO ()
loop subFps subSdl update render state =
  case subFps of
    Nothing ->
      loopWithoutFps subSdl' update render state

    Just subFps' -> do
      now <- monotonicMicros
      loopWithFps subFps' subSdl' update render now state

  where
    subSdl' :: SDL.EventPayload -> Maybe msg
    subSdl' =
      fromMaybe (const Nothing) subSdl

loopWithoutFps ::
     forall msg state.
     (SDL.EventPayload -> Maybe msg)
  -> (msg -> state -> state)
  -> (state -> IO ())
  -> state
  -> IO ()
loopWithoutFps subSdl update render =
  go
  where
    go :: state -> IO ()
    go state = do
      render state
      event <- SDL.waitEvent
      case subSdl (SDL.eventPayload event) of
        Nothing  -> go state
        Just msg -> go (update msg state)

loopWithFps ::
     forall msg state.
     (Int -> msg)
  -> (SDL.EventPayload -> Maybe msg)
  -> (msg -> state -> state)
  -> (state -> IO ())
  -> Int
  -> state
  -> IO ()
loopWithFps subFps subSdl update render =
  go
  where
    go :: Int -> state -> IO ()
    go prev state0 = do
      time0 <- monotonicMicros

      let
        handleAll state =
          SDL.pollEvent >>= \case
            Nothing ->
              pure state

            Just event ->
              case subSdl (SDL.eventPayload event) of
                Nothing ->
                  handleAll state

                Just msg ->
                  handleAll (update msg state)

      state1 <-
        update (subFps (time0 - prev)) <$> handleAll state0

      render state1

      performGC

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
