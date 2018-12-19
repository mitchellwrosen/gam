module Gam.Internal.Texture where

import Gam.Internal.Prelude
import Gam.Internal.RGBA    (RGBA)

import qualified Gam.Internal.RGBA as RGBA

import System.IO.Unsafe (unsafeInterleaveIO)

import qualified Linear
import qualified SDL
import qualified SDL.Image


-- | A small wrapper around an SDL texture, paired with its
-- unsafe-intearleave-IO'd info. Caveat emptor.
data Texture
  = Texture SDL.Texture SDL.TextureInfo

data Opts
  = Opts
  { alpha :: Float
  , flipX :: Bool
  , flipY :: Bool
  , rotate :: Float
  }

width :: Texture -> CInt
width (Texture _ (SDL.TextureInfo _ _ w _)) =
  w

height :: Texture -> CInt
height (Texture _ (SDL.TextureInfo _ _ _ h)) =
  h

fromImageFile ::
     (HasType SDL.Renderer r, MonadReader r m, MonadIO m)
  => FilePath
  -> m Texture
fromImageFile path = do
  surface <- SDL.Image.load path
  texture <- fromSurface surface
  SDL.freeSurface surface
  pure texture

fromSurface ::
     (HasType SDL.Renderer r, MonadIO m, MonadReader r m)
  => SDL.Surface
  -> m Texture
fromSurface surface = do
  texture <- do
    renderer <- view (the @SDL.Renderer)
    SDL.createTextureFromSurface renderer surface

  SDL.textureBlendMode texture $=!
    SDL.BlendAlphaBlend

  info <- liftIO (unsafeInterleaveIO (SDL.queryTexture texture))
  pure (Texture texture info)

render ::
     (HasType SDL.Renderer r, MonadIO m, MonadReader r m)
  => Opts
  -> Maybe (SDL.Rectangle CInt)
  -> Maybe (SDL.Rectangle CInt)
  -> Texture
  -> m ()
render (Opts { alpha, flipX, flipY, rotate }) src dst (Texture texture _) = do
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
