module Gam.Internal.Picture where

import Gam.Internal.P            (P)
import Gam.Internal.Prelude
import Gam.Internal.SpriteSheet  (SpriteSheet(..))
import Gam.Internal.TextureCache (TextureCache)
import Gam.Internal.V            (V)

import qualified Gam.Internal.P            as P
import qualified Gam.Internal.Texture      as Texture
import qualified Gam.Internal.TextureCache as TextureCache

import qualified Linear
import qualified Linear.Affine as Linear
import qualified SDL


data Picture
  = Alpha Float Picture
  | Append Picture Picture
  | Empty
  | FlipX Picture
  | FlipY Picture
  | Rotate Float Picture
  | Sprite SpriteSheet Int
  | Translate V Picture

instance Monoid Picture where
  mempty = Empty
  mappend = (<>)

instance Semigroup Picture where
  Empty <> pic2 = pic2
  pic1 <> Empty = pic1
  pic1 <> pic2 = Append pic1 pic2

render :: SDL.Renderer -> TextureCache -> Picture -> IO ()
render renderer textureCache =
  go 0 0 False False 1
  where
    go :: P -> Float -> Bool -> Bool -> Float -> Picture -> IO ()
    go !point !degrees !flipX !flipY !alpha = \case
      Alpha f pic ->
        go point degrees flipX flipY (alpha * f) pic

      Append pic1 pic2 -> do
        go point degrees flipX flipY alpha pic1
        go point degrees flipX flipY alpha pic2

      Empty ->
        pure ()

      FlipX pic ->
        go point degrees (not flipX) flipY alpha pic

      FlipY pic ->
        go point degrees flipX (not flipY) alpha pic

      Rotate n pic ->
        go point (n + degrees) flipX flipY alpha pic

      Sprite sheet which ->
        renderSprite renderer textureCache sheet which point degrees flipX flipY
          alpha

      Translate v pic ->
        go (P.add v point) degrees flipX flipY alpha pic

renderSprite ::
     SDL.Renderer
  -> TextureCache
  -> SpriteSheet
  -> Int
  -> P
  -> Float
  -> Bool
  -> Bool
  -> Float
  -> IO ()
renderSprite renderer textureCache sheet which point degrees flipX flipY alpha = do
  texture <-
    TextureCache.load
      textureCache
      renderer
      (sheet ^. the @"file")
      (sheet ^. the @"transparent")

  let
    srcRect :: SDL.Rectangle CInt
    srcRect =
      SDL.Rectangle
        (Linear.P (Linear.V2 (nx * sx) (ny * sy)))
        spriteV2

      where
        (ny, nx) =
          fromIntegral which `quotRem` (Texture.width texture `div` sx)

  Texture.render
    renderer
    srcRect
    dstRect
    (realToFrac degrees)
    flipX
    flipY
    alpha
    texture

  where
    (fromIntegral -> sx, fromIntegral -> sy) =
      sheet ^. the @"spriteSize"

    spriteV2 :: Linear.V2 CInt
    spriteV2 =
      Linear.V2 sx sy

    dstRect :: SDL.Rectangle CInt
    dstRect =
      SDL.Rectangle
        (round <$> P.unwrap point)
        spriteV2