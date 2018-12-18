module Gam.Internal.Texture where

import Gam.Internal.Prelude
import Gam.Internal.RGBA    (RGBA)
import Gam.Internal.V       (V)

import qualified Gam.Internal.RGBA as RGBA
import qualified Gam.Internal.V    as V

import qualified Linear
import qualified SDL
import qualified SDL.Image

data Texture
  = Texture
  { size    :: (CInt, CInt)
  , texture :: SDL.Texture
  }

load ::
     (HasType SDL.Renderer r, MonadReader r m, MonadUnliftIO m)
  => FilePath
  -> Maybe RGBA
  -> m Texture
load path transparent =
  bracket
    (SDL.Image.load path)
    SDL.freeSurface
    (fromSurface transparent)

fromSurface ::
     (HasType SDL.Renderer r, MonadIO m, MonadReader r m)
  => Maybe RGBA
  -> SDL.Surface
  -> m Texture
fromSurface transparent surface = do
  Linear.V2 x y <-
    SDL.surfaceDimensions surface

  SDL.surfaceColorKey surface $=!
    (RGBA.toV4 <$> transparent)

  texture <- do
    renderer <- view (the @SDL.Renderer)
    SDL.createTextureFromSurface renderer surface

  SDL.textureBlendMode texture $=!
    SDL.BlendAlphaBlend

  pure Texture
    { size = (x, y)
    , texture = texture
    }

height :: Texture -> CInt
height (Texture (_, y) _) =
  y

width :: Texture -> CInt
width (Texture (x, _) _) =
  x

render ::
     (HasType SDL.Renderer r, MonadIO m, MonadReader r m)
  => SDL.Rectangle CInt
  -> SDL.Rectangle CInt
  -> CDouble
  -> Bool
  -> Bool
  -> Float
  -> Texture
  -> m ()
render src dst degrees flipX flipY alpha (Texture _ texture) = do
  do
    let alphaVar = SDL.textureAlphaMod texture
    let newAlpha = fromIntegral (round (255 * alpha))
    oldAlpha <- SDL.get alphaVar
    when (oldAlpha /= newAlpha) (alphaVar $=! newAlpha)

  renderer <- view (the @SDL.Renderer)

  SDL.copyEx
    renderer
    texture
    (Just src)
    (Just dst)
    degrees
    Nothing
    (Linear.V2 flipX flipY)
