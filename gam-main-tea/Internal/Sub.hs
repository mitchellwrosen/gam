module Internal.Sub where


import Gam.Internal.Prelude

import qualified SDL

import Data.List (nub)


data Sub msg
  = Sub
      (Maybe (Int -> msg))
      (Maybe (SDL.EventPayload -> Maybe msg))
      [SDL.InitFlag]

instance Semigroup (Sub msg) where
  Sub fps1 sdl1 init1 <> Sub fps2 sdl2 init2 =
    Sub (combineFps fps1 fps2) (combineSdl sdl1 sdl2) (nub (init1 ++ init2))

combineFps :: Maybe (Int -> msg) -> Maybe (Int -> msg) -> Maybe (Int -> msg)
combineFps =
  maybe id (const . Just)

combineSdl ::
     Maybe (SDL.EventPayload -> Maybe msg)
  -> Maybe (SDL.EventPayload -> Maybe msg)
  -> Maybe (SDL.EventPayload -> Maybe msg)
combineSdl Nothing g = g
combineSdl f Nothing = f
combineSdl (Just f) (Just g) = Just (\event -> f event <|> g event)
