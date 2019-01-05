module Gam.Main.Tea.Sub
  ( Sub
  , millis
  , mouseMotion
  , mouseClicks
  ) where

import Gam.Internal.Prelude
import Gam.P                (P(..))
import Internal.Sub

import qualified SDL


millis :: (Float -> msg) -> Sub msg
millis f =
  Sub
    (Just f)
    Nothing
    Nothing

mouseMotion :: forall msg. (P -> msg) -> Sub msg
mouseMotion f =
  eventSub $ \case
    SDL.MouseMotionEvent (SDL.MouseMotionEventData _ _ _ (SDL.P (SDL.V2 x y)) _) ->
      Just (f (P (fromIntegral x) (fromIntegral y)))

    _ ->
      Nothing

mouseClicks :: (SDL.MouseButton -> SDL.InputMotion -> P -> msg) -> Sub msg
mouseClicks f =
  eventSub $ \case
    SDL.MouseButtonEvent (SDL.MouseButtonEventData _ motion _ button _ (SDL.P (SDL.V2 x y))) ->
      Just (f button motion (P (fromIntegral x) (fromIntegral y)))

    _ ->
      Nothing

eventSub :: (SDL.EventPayload -> Maybe msg) -> Sub msg
eventSub f =
  Sub Nothing (Just f) Nothing
