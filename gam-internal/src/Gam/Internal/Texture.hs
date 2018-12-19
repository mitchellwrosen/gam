module Gam.Internal.Texture where

import Gam.Internal.Prelude
import Gam.Internal.RGBA    (RGBA)

import qualified Gam.Internal.RGBA as RGBA

import qualified Linear
import qualified SDL
import qualified SDL.Image

data Opts
  = Opts
  { alpha :: Float
  , flipX :: Bool
  , flipY :: Bool
  , rotate :: Float
  }

load ::
     (HasType SDL.Renderer r, MonadReader r m, MonadIO m)
  => FilePath
  -> Maybe RGBA
  -> m SDL.Texture
load path transparent = do
  surface <- SDL.Image.load path
  SDL.surfaceColorKey surface $=! (RGBA.toV4 <$> transparent)
  texture <- fromSurface surface
  SDL.freeSurface surface
  pure texture

fromSurface ::
     (HasType SDL.Renderer r, MonadIO m, MonadReader r m)
  => SDL.Surface
  -> m SDL.Texture
fromSurface surface = do
  texture <- do
    renderer <- view (the @SDL.Renderer)
    SDL.createTextureFromSurface renderer surface

  SDL.textureBlendMode texture $=!
    SDL.BlendAlphaBlend

  pure texture

render ::
     (HasType SDL.Renderer r, MonadIO m, MonadReader r m)
  => Opts
  -> Maybe (SDL.Rectangle CInt)
  -> Maybe (SDL.Rectangle CInt)
  -> SDL.Texture
  -> m ()
render (Opts { alpha, flipX, flipY, rotate }) src dst texture = do
  do
    let alphaVar = SDL.textureAlphaMod texture
    let newAlpha = round (255 * alpha)
    oldAlpha <- SDL.get alphaVar
    when (oldAlpha /= newAlpha) (alphaVar $=! newAlpha)

  renderer <- view (the @SDL.Renderer)

  SDL.copyEx
    renderer
    texture
    src
    dst
    (realToFrac rotate)
    Nothing
    (Linear.V2 flipX flipY)
