module Internal.Music
  ( MusicState(..)
  , adjustMusic
  ) where

import Gam.Internal.Music   (Music)
import Gam.Internal.Prelude

import qualified Gam.Internal.Music as Music

import qualified SDL.Mixer

-- Mini music state machine.
--
-- When we are fading out music, it doesn't matter what the current state says
-- what the music should be - we keep fading.
data MusicState
  = NotPlaying
  | FadingOut (IO ()) -- Action to free music
  | Playing PlayingMusic

data PlayingMusic
  = PlayingMusic Music SDL.Mixer.Music

adjustMusic ::
     MusicState
  -> Maybe Music
  -> IO MusicState
adjustMusic oldMusicState newMusicSettings =
  case oldMusicState of
    NotPlaying ->
      fromSilence newMusicSettings

    FadingOut free ->
      fromFadingOut free newMusicSettings

    Playing playing ->
      fromPlaying playing newMusicSettings

fromSilence :: Maybe Music -> IO MusicState
fromSilence = \case
  Nothing ->
    pure NotPlaying

  Just settings ->
    play settings

fromFadingOut :: IO () -> Maybe Music -> IO MusicState
fromFadingOut free settings =
  SDL.Mixer.playingMusic >>= \case
    False -> do
      free
      fromSilence settings

    True ->
      pure (FadingOut free)

fromPlaying :: PlayingMusic -> Maybe Music -> IO MusicState
fromPlaying playing =
  maybe
    (fromPlayingToSilence playing)
    (fromPlayingToPlaying playing)

fromPlayingToSilence :: PlayingMusic -> IO MusicState
fromPlayingToSilence (PlayingMusic settings music) = do
  case Music.fadeOut settings of
    0 -> do
      SDL.Mixer.haltMusic
      SDL.Mixer.free music
      pure NotPlaying

    n ->
      SDL.Mixer.fadeOutMusic n >>= \case
        False -> do
          SDL.Mixer.free music
          pure NotPlaying

        True ->
          pure (FadingOut (SDL.Mixer.free music))

fromPlayingToPlaying :: PlayingMusic -> Music -> IO MusicState
fromPlayingToPlaying playing@(PlayingMusic oldSettings _) settings =
  if Music.file oldSettings == Music.file settings
    then fromPlayingToPlayingSame playing settings
    else fromPlayingToPlayingDifferent playing settings

-- Playing the same music, but perhaps adjust the volume.
fromPlayingToPlayingSame :: PlayingMusic -> Music -> IO MusicState
fromPlayingToPlayingSame playing@(PlayingMusic oldSettings music) settings = do
  if Music.volume oldSettings == Music.volume settings
    then
      pure (Playing playing)

    else do
      SDL.Mixer.setMusicVolume (Music.volume settings)
      pure (Playing (PlayingMusic settings music))

fromPlayingToPlayingDifferent :: PlayingMusic -> Music -> IO MusicState
fromPlayingToPlayingDifferent (PlayingMusic oldSettings oldMusic) settings = do
  case Music.fadeOut oldSettings of
    0 -> do
      SDL.Mixer.haltMusic
      SDL.Mixer.free oldMusic
      play settings

    n -> do
      SDL.Mixer.fadeOutMusic n >>= \case
        False -> do
          SDL.Mixer.free oldMusic
          play settings

        True ->
          pure (FadingOut (SDL.Mixer.free oldMusic))

play :: Music -> IO MusicState
play settings = do
  music <- SDL.Mixer.load (unhashed (Music.file settings))
  SDL.Mixer.setMusicVolume (Music.volume settings)
  doPlay SDL.Mixer.Forever music
  pure (Playing (PlayingMusic settings music))

  where
    doPlay :: SDL.Mixer.Times -> SDL.Mixer.Music -> IO ()
    doPlay =
      case Music.fadeIn settings of
        0 -> SDL.Mixer.playMusic
        n -> SDL.Mixer.fadeInMusic n

