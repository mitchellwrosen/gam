module Gam.Main.Tea.Sub
  ( Sub
  , millis
  , mouseMotion
  , mouseClicks
  ) where

import Gam.Internal.Prelude
import Gam.Internal.P (P(P_))
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
    SDL.MouseMotionEvent (SDL.MouseMotionEventData _ _ _ p _) ->
      Just (f (P_ (fromIntegral <$> p)))

    _ ->
      Nothing

mouseClicks :: (SDL.MouseButton -> SDL.InputMotion -> P -> msg) -> Sub msg
mouseClicks f =
  eventSub $ \case
    SDL.MouseButtonEvent (SDL.MouseButtonEventData _ motion _ button _ p) ->
      Just (f button motion (P_ (fromIntegral <$> p)))

    _ ->
      Nothing

eventSub :: (SDL.EventPayload -> Maybe msg) -> Sub msg
eventSub f =
  Sub Nothing (Just f) Nothing
