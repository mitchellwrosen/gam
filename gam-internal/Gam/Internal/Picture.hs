module Gam.Internal.Picture where

import Gam.Internal.P            (P)
import Gam.Internal.Prelude
import Gam.Internal.SpriteSheet  (SpriteSheet(..))
import Gam.Internal.TextureCache (TextureCache)
import Gam.Internal.V            (V)

import qualified Gam.Internal.P            as P
import qualified Gam.Internal.Texture      as Texture
import qualified Gam.Internal.TextureCache as TextureCache
import qualified Gam.Internal.V            as V

import qualified Linear
import qualified Linear.Affine as Linear
import qualified SDL


data Picture
  = Empty
  | Sprite SpriteSheet Int
  | Translate V Picture
  | Rotate Float Picture
  | FlipX Picture
  | FlipY Picture
  | Append Picture Picture

instance Monoid Picture where
  mempty = Empty
  mappend = (<>)

instance Semigroup Picture where
  Empty <> pic2 = pic2
  pic1 <> Empty = pic1
  pic1 <> pic2 = Append pic1 pic2

sprite :: SpriteSheet -> Int -> Picture
sprite sheet which =
  Sprite sheet which

translate :: V -> Picture -> Picture
translate v = \case
  Empty ->
    Empty

  Translate w pic ->
    Translate (V.add w v) pic

  pic@Sprite{} -> Translate v pic
  pic@Rotate{} -> Translate v pic
  pic@Append{} -> Translate v pic

rotate :: Float -> Picture -> Picture
rotate n = \case
  Empty ->
    Empty

  Rotate m pic ->
    Rotate (n + m) pic

  pic@Sprite{}    -> Rotate n pic
  pic@Translate{} -> Rotate n pic
  pic@Append{}    -> Rotate n pic

render :: SDL.Renderer -> TextureCache -> Picture -> IO ()
render renderer textureCache =
  go 0 0 False False
  where
    go :: P -> Float -> Bool -> Bool -> Picture -> IO ()
    go !point !degrees !flipX !flipY = \case
      Empty ->
        pure ()

      Sprite sheet which ->
        renderSprite renderer textureCache sheet which point degrees flipX flipY

      Translate v pic ->
        go (P.add v point) degrees flipX flipY pic

      Rotate n pic ->
        go point (n + degrees) flipX flipY pic

      FlipX pic ->
        go point degrees (not flipX) flipY pic

      FlipY pic ->
        go point degrees flipX (not flipY) pic

      Append pic1 pic2 -> do
        go point degrees flipX flipY pic1
        go point degrees flipX flipY pic2

renderSprite ::
     SDL.Renderer
  -> TextureCache
  -> SpriteSheet
  -> Int
  -> P
  -> Float
  -> Bool
  -> Bool
  -> IO ()
renderSprite renderer textureCache sheet which point degrees flipX flipY = do
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
