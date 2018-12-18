module Gam.Main.Tea where

import Gam.Internal.Music   (Music)
import Gam.Internal.Prelude
import Gam.Internal.Window  (Window)
import Internal.Sub         (Sub(..))

import qualified Gam.Internal.FontCache        as FontCache
import qualified Gam.Internal.Music            as Music
import qualified Gam.Internal.Render           as Render
import qualified Gam.Internal.SpriteSheetCache as SpriteSheetCache
import qualified Gam.Internal.Window           as Window

import Control.Concurrent (threadDelay)
import GHC.Clock
import System.Mem         (performGC)

import qualified SDL
import qualified SDL.Font
import qualified SDL.Mixer


main ::
     forall msg state.
     state
  -> (state -> Sub msg)
  -> (msg -> state -> state)
  -> (state -> Window)
  -> IO ()
main state subs update render = do
  SDL.initializeAll
  SDL.Font.initialize
  SDL.Mixer.openAudio SDL.Mixer.defaultAudio 2048

  window <-
    Window.new (render state)

  renderer <-
    SDL.createRenderer window (-1) SDL.RendererConfig
      { SDL.rendererType = SDL.AcceleratedVSyncRenderer
      , SDL.rendererTargetTexture = False
      }

  fontCache <-
    FontCache.new

  spriteSheetCache <-
    SpriteSheetCache.new

  let
    doRender :: state -> IO ()
    doRender =
      render >>>
      Window.render >>>
      Render.run window renderer fontCache spriteSheetCache

  now <-
    monotonicMicros

  mainLoop
    subs
    update
    doRender
    now
    state

mainLoop
  :: forall msg state.
     (state -> Sub msg)
  -> (msg -> state -> state)
  -> (state -> IO ())
  -> Int
  -> state
  -> IO ()
mainLoop subs update render =
  go NotPlayingMusic

  where
    go :: PlayingMusic -> Int -> state -> IO ()
    go oldPlayingMusic prevTime state0 = do
      time0 <- monotonicMicros

      let
        currentSubs :: Sub msg
        currentSubs =
          subs state0

      -- Adjust the current music
      newPlayingMusic <-
        adjustMusic oldPlayingMusic (music currentSubs)

      -- Process SDL events since last frame
      state1 <-
        processEvents (event currentSubs) update state0

      -- Step the game forward per how much time has passed between the
      -- beginning of the previous loop and the beginning of this loop
      let
        state2 =
          stepGame (fps currentSubs) (time0 - prevTime) update state1

      -- Render the updated game state
      render state2

      -- GC every frame
      performGC

      -- Sleep until the next frame
      time1 <- monotonicMicros
      threadDelay (time0 + microsPerFrame - time1)
      go newPlayingMusic time0 state2

-- Mini music state machine.
--
-- When we are fading out music, it doesn't matter what the current state says
-- what the music should be. We check every frame to see if the music has faded
-- out, then switch to the 'NotPlaying' music state.
data PlayingMusic
  = NotPlayingMusic
  | FadingOutMusic SDL.Mixer.Music
  | PlayingMusic Music SDL.Mixer.Music

adjustMusic ::
     PlayingMusic
  -> Maybe Music
  -> IO PlayingMusic
adjustMusic oldPlayingMusic newMusicSettings =
  case oldPlayingMusic of
    NotPlayingMusic ->
      case newMusicSettings of
        Nothing ->
          pure NotPlayingMusic

        Just settings -> do
          music <- play settings
          pure (PlayingMusic settings music)

    FadingOutMusic music ->
      SDL.Mixer.playingMusic >>= \case
        False -> do
          SDL.Mixer.free music
          pure NotPlayingMusic

        True ->
          pure (FadingOutMusic music)

    PlayingMusic oldSettings oldMusic ->
      case newMusicSettings of
        Nothing -> do
          case Music.fadeOut oldSettings of
            0 -> do
              SDL.Mixer.haltMusic
              SDL.Mixer.free oldMusic
              pure NotPlayingMusic

            n ->
              SDL.Mixer.fadeOutMusic n >>= \case
                False -> do
                  SDL.Mixer.free oldMusic
                  pure NotPlayingMusic

                True ->
                  pure (FadingOutMusic oldMusic)


        Just settings -> do
          if Music.file oldSettings == Music.file settings
            then do
              when (Music.volume oldSettings /= Music.volume settings)
                (SDL.Mixer.setMusicVolume (Music.volume settings))

              pure (PlayingMusic settings oldMusic)

            else do
              case Music.fadeOut oldSettings of
                0 -> do
                  SDL.Mixer.haltMusic
                  SDL.Mixer.free oldMusic
                  music <- play settings
                  pure (PlayingMusic settings music)

                n -> do
                  SDL.Mixer.fadeOutMusic n >>= \case
                    False -> do
                      SDL.Mixer.free oldMusic
                      music <- play settings
                      pure (PlayingMusic settings music)

                    True ->
                      pure (FadingOutMusic oldMusic)

  where
    play :: Music -> IO SDL.Mixer.Music
    play settings = do
      music <- SDL.Mixer.load (Music.file settings)
      SDL.Mixer.setMusicVolume (Music.volume settings)
      case Music.fadeIn settings of
        0 -> SDL.Mixer.playMusic SDL.Mixer.Forever music
        n -> SDL.Mixer.fadeInMusic n SDL.Mixer.Forever music
      pure music

processEvents ::
     forall msg state.
     Maybe (SDL.EventPayload -> Maybe msg)
  -> (msg -> state -> state)
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
                  loop (update msg state)
      in
        loop state0

stepGame ::
     Maybe (Int -> msg)
  -> Int
  -> (msg -> state -> state)
  -> state
  -> state
stepGame sub dt update =
  case sub of
    Nothing -> id
    Just f  -> update (f dt)

monotonicMicros :: IO Int
monotonicMicros = do
  ns <- getMonotonicTimeNSec
  pure (fromIntegral (ns `div` 1000))

-- 60 fps
microsPerFrame :: Int
microsPerFrame =
  16667
