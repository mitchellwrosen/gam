module Gam.Internal.Texture where

import Gam.Internal.P       (P(..))
import Gam.Internal.Prelude
import Gam.Internal.V       (V(..))

import Foreign.Ptr      (Ptr, castPtr)
import System.IO.Unsafe (unsafeInterleaveIO)

import qualified SDL
import qualified SDL.Image


-- | A small wrapper around an SDL texture, paired with its
-- unsafe-intearleave-IO'd info. Caveat emptor.
data Texture
  = Texture SDL.Texture ~SDL.TextureInfo

data Opts
  = Opts
  { alpha :: Float
  , clip :: Maybe (SDL.Rectangle CInt)
  , flipX :: Bool
  , flipY :: Bool
  , position :: P
  , rotation :: Float
  , scale :: (Float, Float)
  }

width :: Texture -> CInt
width (Texture _ (SDL.TextureInfo _ _ w _)) =
  w

height :: Texture -> CInt
height (Texture _ (SDL.TextureInfo _ _ _ h)) =
  h

forCairo ::
     ( HasType SDL.Renderer r
     , MonadIO m
     , MonadReader r m
     )
  => V
  -> m Texture
forCairo (V (round -> width) (round -> height)) = do
  renderer <-
    view (the @SDL.Renderer)

  texture <-
    SDL.createTexture
      renderer
      SDL.ARGB8888
      SDL.TextureAccessStreaming
      (SDL.V2 width height)

  fromTexture texture

-- | Load a 'Texture' from an image 'FilePath'.
fromImageFile ::
     ( HasType SDL.Renderer r
     , MonadIO m
     , MonadReader r m
     )
  => FilePath
  -> m Texture
fromImageFile path =
  SDL.Image.load path >>= fromSurface

-- | Create a 'Texture' from an SDL surface, and free the surface.
fromSurface ::
     ( HasType SDL.Renderer r
     , MonadIO m
     , MonadReader r m
     )
  => SDL.Surface
  -> m Texture
fromSurface surface = do
  texture <- do
    renderer <- view (the @SDL.Renderer)
    SDL.createTextureFromSurface renderer surface

  SDL.freeSurface surface

  fromTexture texture

fromTexture :: MonadIO m => SDL.Texture -> m Texture
fromTexture texture = do
  SDL.textureBlendMode texture $=!
    SDL.BlendAlphaBlend

  info <- liftIO (unsafeInterleaveIO (SDL.queryTexture texture))

  pure (Texture texture info)

lock :: MonadIO m => Texture -> m (Ptr a, CInt)
lock (Texture texture _) = do
  (pixels, pitch) <- SDL.lockTexture texture Nothing
  pure (castPtr pixels, pitch)

unlock :: MonadIO m => Texture -> m ()
unlock (Texture texture _) =
  SDL.unlockTexture texture

destroy :: MonadIO m => Texture -> m ()
destroy (Texture texture _) =
  SDL.destroyTexture texture

render ::
     ( HasType SDL.Renderer r
     , MonadIO m
     , MonadReader r m
     )
  => Opts
  -> Texture
  -> m ()
render
    (Opts { alpha, clip, flipX, flipY, position = P tx ty, rotation, scale = (scaleX, scaleY) })
    (Texture texture (SDL.TextureInfo _ _ tw th)) = do

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
    (Just (SDL.Rectangle (SDL.P (SDL.V2 (round tx) (round ty))) (SDL.V2 dx dy)))
    (realToFrac rotation)
    Nothing
    (SDL.V2 flipX flipY)

  where
    sx, sy :: CInt
    (sx, sy) =
      maybe
        (tw, th)
        (\(SDL.Rectangle _ (SDL.V2 w h)) -> (w, h))
        clip

    dx, dy :: CInt
    dx = if scaleX == 1 then sx else round (scaleX * fromIntegral sx)
    dy = if scaleY == 1 then sy else round (scaleY * fromIntegral sy)
