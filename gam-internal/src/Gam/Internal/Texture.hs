module Gam.Internal.Texture where

import Gam.Internal.Prelude
import Gam.Internal.RGBA    (RGBA)
import Gam.Internal.V       (V(..))

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
  , clip :: Maybe (SDL.Rectangle CInt)
  , flipX :: Bool
  , flipY :: Bool
  , rotate :: Float
  , scale :: (Float, Float)
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
  -> V
  -> Texture
  -> m ()
render (Opts { alpha, clip, flipX, flipY, rotate, scale = (scaleX, scaleY) })
    translate (Texture texture (SDL.TextureInfo _ _ tw th)) = do

  do
    let alphaVar = SDL.textureAlphaMod texture
    let newAlpha = round (255 * alpha)
    oldAlpha <- SDL.get alphaVar
    when (oldAlpha /= newAlpha) (alphaVar $=! newAlpha)

  renderer <-
    view (the @SDL.Renderer)

  SDL.copyEx
    renderer
    texture
    clip
    (Just (SDL.Rectangle point (SDL.V2 dx dy)))
    (realToFrac rotate)
    Nothing
    (Linear.V2 flipX flipY)

  where
    sx, sy :: CInt
    (sx, sy) =
      case clip of
        Nothing                             -> (tw, th)
        Just (SDL.Rectangle _ (SDL.V2 w h)) -> (w, h)

    dx, dy :: CInt
    dx = if scaleX == 1 then sx else round (scaleX * fromIntegral sx)
    dy = if scaleY == 1 then sy else round (scaleY * fromIntegral sy)

    point :: SDL.Point SDL.V2 CInt
    point =
      case translate of
        V tx ty ->
          SDL.P (SDL.V2 (round tx) (round ty))
