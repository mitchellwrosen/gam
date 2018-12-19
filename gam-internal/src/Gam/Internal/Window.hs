module Gam.Internal.Window where

import Gam.Internal.FontCache        (FontCache)
import Gam.Internal.Picture          (Picture)
import Gam.Internal.Prelude
import Gam.Internal.RGBA             (RGBA)
import Gam.Internal.SpriteSheetCache (SpriteSheetCache)

import qualified Gam.Internal.Picture as Picture
import qualified Gam.Internal.RGBA    as RGBA

import qualified Linear
import qualified SDL

data Window
  = Window
  { background :: RGBA
  , pictures :: [Picture]
  , scale :: (Float, Float)
  , size :: (Int, Int)
  , title :: Text
  } deriving stock (Generic)

new :: MonadIO m => Window -> m SDL.Window
new (Window { size, title }) =
  SDL.createWindow title windowConfig
  where
    windowConfig :: SDL.WindowConfig
    windowConfig =
      SDL.defaultWindow
        { SDL.windowInitialSize =
            case size of
              (x, y) ->
                Linear.V2 (fromIntegral x) (fromIntegral y)
        }

render ::
     ( HasType FontCache r
     , HasType SDL.Renderer r
     , HasType SDL.Window r
     , HasType SpriteSheetCache r
     , MonadIO m
     , MonadReader r m
     )
  => Window
  -> m ()
render (Window { background, pictures, scale, size, title }) = do
  window <- view (the @SDL.Window)
  renderer <- view (the @SDL.Renderer)

  do
    let newBackground = RGBA.toV4 background
    let backgroundVar = SDL.rendererDrawColor renderer
    oldBackground <- SDL.get backgroundVar
    when (oldBackground /= newBackground) (backgroundVar $=! newBackground)

  do
    let newSize =
          case size of
            (x, y) ->
              Linear.V2 (fromIntegral x) (fromIntegral y)
    let windowSizeVar = SDL.windowSize window
    oldSize <- SDL.get windowSizeVar
    when (oldSize /= newSize) (windowSizeVar $=! newSize)

  do
    let windowTitleVar = SDL.windowTitle window
    oldTitle <- SDL.get windowTitleVar
    when (oldTitle /= title) (windowTitleVar $=! title)

  SDL.clear renderer
  for_ pictures Picture.render
  SDL.present renderer
