module Gam.Internal.Texture where

import Gam.Internal.Prelude
import Gam.Internal.RGBA    (RGBA)
import Gam.Internal.V       (V)

import qualified Gam.Internal.RGBA as RGBA
import qualified Gam.Internal.V    as V

import qualified SDL
import qualified SDL.Image
import qualified Linear

data Texture
  = Texture
  { size :: (CInt, CInt)
  , texture :: SDL.Texture
  }

load :: SDL.Renderer -> FilePath -> Maybe RGBA -> IO Texture
load renderer path transparent =
  bracket
  (SDL.Image.load path)
  SDL.freeSurface
  (fromSurface renderer transparent)

fromSurface :: SDL.Renderer -> Maybe RGBA -> SDL.Surface -> IO Texture
fromSurface renderer transparent surface = do
  Linear.V2 x y <-
    SDL.surfaceDimensions surface

  SDL.surfaceColorKey surface $=!
    (RGBA.toV4 <$> transparent)

  texture <-
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
     SDL.Renderer
  -> SDL.Rectangle CInt
  -> SDL.Rectangle CInt
  -> CDouble
  -> Bool
  -> Bool
  -> Float
  -> Texture
  -> IO ()
render renderer src dst degrees flipX flipY alpha (Texture _ texture) = do
  do
    let alphaVar = SDL.textureAlphaMod texture
    let newAlpha = fromIntegral (round (255 * alpha))
    oldAlpha <- SDL.get alphaVar
    when (oldAlpha /= newAlpha) (alphaVar $=! newAlpha)

  SDL.copyEx
    renderer
    texture
    (Just src)
    (Just dst)
    degrees
    Nothing
    (Linear.V2 flipX flipY)
