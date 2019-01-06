module Gam.Internal.Textual
  ( render
  ) where

import Gam.Internal.FontCache         (FontCache)
import Gam.Internal.P                 (P)
import Gam.Internal.Prelude
import Gam.Internal.RenderedTextCache (RenderedTextCache)
import Gam.Internal.RGBA              (RGBA)
import Gam.Internal.TextStyle         (TextStyle(..))
import Gam.Internal.Typeface          (Typeface)

import qualified Gam.Internal.RenderedTextCache as RenderedTextCache
import qualified Gam.Internal.Texture           as Texture
import qualified Internal.Form                  as Form

import qualified SDL

render ::
     ( HasType FontCache r
     , HasType RenderedTextCache r
     , HasType SDL.Renderer r
     , MonadIO m
     , MonadReader r m
     )
  => Form.Opts
  -> TextStyle
  -> Text
  -> m ()
render opts style text = do
  texture <-
    RenderedTextCache.load style text

  Texture.render
    (Texture.Opts
      { Texture.alpha = Form.alpha opts
      , Texture.clip = Nothing
      , Texture.flipX = False
      , Texture.flipY = False
      , Texture.position = Form.position opts
      , Texture.rotation = Form.rotation opts
      , Texture.scale = Form.scale opts
      })
    texture
