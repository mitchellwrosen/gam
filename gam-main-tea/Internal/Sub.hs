module Internal.Sub where


import Gam.Internal.Prelude
import Gam.Internal.Music (Music)

import qualified SDL

import Data.List (nub)
import Data.Monoid (First(..))


data Sub msg
  = Sub
  { fps :: Maybe (Int -> msg)
  , event :: Maybe (SDL.EventPayload -> Maybe msg)
  , music :: Maybe Music
  }

instance Semigroup (Sub msg) where
  Sub fps1 event1 music1 <> Sub fps2 event2 music2 =
    Sub
      (getFirst (First fps1 <> First fps2))
      (combineSdl event1 event2)
      (getFirst (First music1 <> First music2))

combineSdl ::
     Maybe (SDL.EventPayload -> Maybe msg)
  -> Maybe (SDL.EventPayload -> Maybe msg)
  -> Maybe (SDL.EventPayload -> Maybe msg)
combineSdl Nothing g = g
combineSdl f Nothing = f
combineSdl (Just f) (Just g) = Just (\event -> f event <|> g event)
