module Gam.Main.Tea
  ( main
  , Sub
  ) where

import Gam.Internal.Config     (Config)
import Gam.Internal.FrameCount (FrameCount)
import Gam.Internal.Music      (Music)
import Gam.Internal.Prelude
import Gam.Internal.Window     (Window)
import Internal.Music
import Internal.Sub            (Sub(..))

import qualified Gam.Internal.FontCache         as FontCache
import qualified Gam.Internal.Music             as Music
import qualified Gam.Internal.Render            as Render
import qualified Gam.Internal.RenderedTextCache as RenderedTextCache
import qualified Gam.Internal.SpriteSheetCache  as SpriteSheetCache
import qualified Gam.Internal.Window            as Window

import Control.Concurrent (threadDelay)
import GHC.Clock
import System.Mem         (performGC)
import Text.Printf        (printf)

import qualified SDL
import qualified SDL.Font
import qualified SDL.Mixer

debug :: Bool
debug = False

main ::
     forall msg state.
     Config
  -> state
  -> (state -> Sub msg)
  -> (msg -> state -> IO state)
  -> (state -> Window)
  -> IO ()
main config state subs update render = do
  SDL.initializeAll
  SDL.Font.initialize
  SDL.Mixer.openAudio SDL.Mixer.defaultAudio 2048

  window <-
    Window.new config

  renderer <-
    SDL.createRenderer window (-1) SDL.RendererConfig
      { SDL.rendererType = SDL.AcceleratedVSyncRenderer
      , SDL.rendererTargetTexture = False
      }

  fontCache <-
    FontCache.new

  renderedTextCache <-
    RenderedTextCache.new

  spriteSheetCache <-
    SpriteSheetCache.new

  frameCountRef <-
    newIORef 0

  let
    doRender :: state -> IO ()
    doRender =
      render >>>
      Window.render >>>
      Render.run window renderer fontCache renderedTextCache spriteSheetCache
        frameCountRef

  now <-
    getMonotonicTimeNSec

  mainLoop
    frameCountRef
    subs
    update
    doRender
    now
    state

mainLoop
  :: forall msg state.
     IORef FrameCount
  -> (state -> Sub msg)
  -> (msg -> state -> IO state)
  -> (state -> IO ())
  -> Word64
  -> state
  -> IO ()
mainLoop frameCountRef subs update render =
  go NotPlaying

  where
    go :: MusicState -> Word64 -> state -> IO ()
    go oldMusicState prevTime state0 = do
      time0 <- getMonotonicTimeNSec

      modifyIORef' frameCountRef (+1)

      let
        currentSubs :: Sub msg
        currentSubs =
          subs state0

      -- Adjust the current music
      newPlayingMusic <-
        adjustMusic oldMusicState (music currentSubs)

      -- Process SDL events since last frame
      state1 <-
        processEvents (event currentSubs) update state0

      -- Step the game forward per how much time has passed between the
      -- beginning of the previous loop and the beginning of this loop
      state2 <-
        stepGame (fps currentSubs) (time0 - prevTime) update state1

      -- Render the updated game state
      render state2 *-- "render"

      -- GC every frame
      performGC *-- "gc"

      -- Sleep until the next frame
      time1 <- getMonotonicTimeNSec

      let
        elapsed :: Word64
        elapsed =
          time1 - time0

      if elapsed <= nanosPerFrame
        then
          sleepNanos (nanosPerFrame - elapsed)

        else do
          let
            (skipped, elapsed') =
              elapsed `quotRem` nanosPerFrame
          printf "[WARNING] Skipping %d frame(s)\n" skipped
          sleepNanos (nanosPerFrame - elapsed')

      go newPlayingMusic time0 state2

processEvents ::
     forall msg state.
     Maybe (SDL.EventPayload -> Maybe msg)
  -> (msg -> state -> IO state)
  -> state
  -> IO state
processEvents sub update state0 =
  case sub of
    Nothing -> do
      _ <- SDL.pollEvents
      pure state0

    Just f ->
      let
        loop :: state -> IO state
        loop !state =
          SDL.pollEvent >>= \case
            Nothing ->
              pure state

            Just event ->
              case f (SDL.eventPayload event) of
                Nothing ->
                  loop state

                Just msg ->
                  update msg state >>= loop
      in
        loop state0

stepGame ::
     Maybe (Float -> msg)
  -> Word64
  -> (msg -> state -> IO state)
  -> state
  -> IO state
stepGame sub nanos update =
  case sub of
    Nothing -> pure
    Just f  -> update (f (fromIntegral nanos / 1000000))

(*--) :: IO a -> String -> IO a
(*--) action str =
  if debug
    then do
      t0 <- getMonotonicTimeNSec
      result <- action
      t1 <- getMonotonicTimeNSec
      putStrLn (str ++ ": " ++ show (t1 - t0) ++ "us")
      pure result
    else
      action

nanosPerFrame :: Word64
nanosPerFrame =
  fps 30
  where
    fps :: Float -> Word64
    fps n =
      round (1000000000 / n)

sleepNanos :: Word64 -> IO ()
sleepNanos nanos =
  threadDelay (fromIntegral (nanos `div` 1000))
