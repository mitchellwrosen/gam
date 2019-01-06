module Gam.Internal.Collage
  ( Collage(..)
  , render
  ) where

import Gam.Internal.FontCache         (FontCache)
import Gam.Internal.FrameCount        (FrameCount)
import Gam.Internal.Prelude
import Gam.Internal.RenderedTextCache (RenderedTextCache)
import Gam.Internal.SpriteSheetCache  (SpriteSheetCache)
import Internal.Collage               (Collage(..))

import qualified Gam.Internal.Form as Form (render)
import qualified Gam.Internal.P    as P

import qualified SDL

render ::
     ( HasType FontCache r
     , HasType (IORef FrameCount) r
     , HasType RenderedTextCache r
     , HasType SDL.Renderer r
     , HasType SpriteSheetCache r
     , MonadIO m
     , MonadReader r m
     )
  => Collage
  -> m ()
render (Collage { position, forms }) =
  for_ forms $ \(opts, form) ->
    Form.render (opts & the @"position" %~ P.add (P.asV position)) form
