module Gam.Internal.Sprite
  ( Sprite(..)
  , module Gam.Internal.Sprite
  ) where

import Gam.Internal.FrameCount       (FrameCount(..))
import Gam.Internal.P                (P)
import Gam.Internal.Prelude
import Gam.Internal.SpriteSheet      (SpriteSheet)
import Gam.Internal.SpriteSheetCache (SpriteSheetCache)
import Internal.Sprite               (Sprite(..))

import qualified Gam.Internal.P                as P
import qualified Gam.Internal.SpriteSheet      as SpriteSheet
import qualified Gam.Internal.SpriteSheetCache as SpriteSheetCache
import qualified Gam.Internal.Texture          as Texture
import qualified Internal.Form                 as Form

import qualified SDL


rect :: FrameCount -> Sprite -> SDL.Rectangle CInt
rect (FrameCount n) (Sprite { sheet, indices }) =
  SDL.Rectangle
    (SDL.P (SDL.V2 (fromIntegral (nx * sx)) (fromIntegral (ny * sy))))
    (SDL.V2 (fromIntegral sx) (fromIntegral sy))
  where
    (nx, ny) = indices !! (fromIntegral n `rem` length indices)
    (sx, sy) = SpriteSheet.spriteSize sheet

render ::
     ( HasType SDL.Renderer r
     , HasType SpriteSheetCache r
     , HasType (IORef FrameCount) r
     , MonadIO m
     , MonadReader r m
     )
  => Form.Opts
  -> Sprite
  -> m ()
render opts sprite = do
  texture <-
    SpriteSheetCache.load (SpriteSheet.file (sheet sprite))

  frameCount <-
    liftIO . readIORef =<< view (the @(IORef FrameCount))

  Texture.render
    (Texture.Opts
      { Texture.alpha = Form.alpha opts
      , Texture.clip = Just (rect frameCount sprite)
      , Texture.flipX = False
      , Texture.flipY = False
      , Texture.position = Form.position opts
      , Texture.rotation = Form.rotation opts
      , Texture.scale = Form.scale opts
      })
    texture
