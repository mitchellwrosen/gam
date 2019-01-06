module Gam.Internal.Form
  ( Form(..)
  , Opts(..)
  , render
  ) where

import Gam.Internal.FontCache         (FontCache)
import Gam.Internal.FrameCount        (FrameCount)
import Gam.Internal.Prelude
import Gam.Internal.RenderedTextCache (RenderedTextCache)
import Gam.Internal.RGB               (RGB(..))
import Gam.Internal.SpriteSheetCache  (SpriteSheetCache)
import Gam.Internal.V                 (V(..))
import Internal.Collage               (Form(..))
import Internal.Form                  (Opts(..))

import qualified Gam.Internal.Sprite  as Sprite
import qualified Gam.Internal.Textual as Textual
import qualified Gam.Internal.Texture as Texture

import qualified Graphics.Rendering.Cairo as Cairo
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
  => Opts
  -> Form
  -> m ()
render opts = \case
  -- TODO: cache rectangles somehow, don't create texture every frame.
  Rectangle (RGB r g b) diagonal@(V x y) -> do
    texture <-
      Texture.forCairo diagonal

    (pixels, pitch) <-
      Texture.lock texture

    liftIO
      (Cairo.withImageSurfaceForData
        pixels
        Cairo.FormatARGB32
        (fromIntegral (Texture.width texture))
        (fromIntegral (Texture.height texture))
        (fromIntegral pitch)
        (\surface ->
          Cairo.renderWith surface $ do
            Cairo.setSourceRGBA
              (fromIntegral r / 255)
              (fromIntegral g / 255)
              (fromIntegral b / 255)
              1
            Cairo.rectangle 0 0 (realToFrac x) (realToFrac y)
            Cairo.fill))

    Texture.unlock texture
    Texture.render
      (Texture.Opts
        { Texture.alpha = alpha opts
        , Texture.clip = Nothing
        , Texture.flipX = False
        , Texture.flipY = False
        , Texture.position = position opts
        , Texture.rotation = rotation opts
        , Texture.scale = scale opts
        })
      texture
    Texture.destroy texture

  Sprite sprite ->
    Sprite.render
      opts
      sprite

  Textual style text ->
    Textual.render
      opts
      style
      text
