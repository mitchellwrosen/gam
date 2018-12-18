module Gam.Internal.Window where

import Gam.Internal.FontCache        (FontCache)
import Gam.Internal.Picture          (Picture)
import Gam.Internal.Prelude
import Gam.Internal.RGBA             (RGBA)
import Gam.Internal.SpriteSheetCache (SpriteSheetCache)
import Gam.Internal.V                (V(..))

import qualified Gam.Internal.Picture as Picture
import qualified Gam.Internal.RGBA    as RGBA
import qualified Gam.Internal.V       as V

import qualified SDL

data Window
  = Window
  { title      :: Text
  , size       :: V
  , background :: RGBA
  , picture    :: Picture
  } deriving stock (Generic)

new :: MonadIO m => Window -> m SDL.Window
new window =
  SDL.createWindow (window ^. the @"title") windowConfig
  where
    windowConfig :: SDL.WindowConfig
    windowConfig =
      SDL.defaultWindow
        { SDL.windowInitialSize =
            round <$> V.toV2 (window ^. the @"size")
        }

render ::
     ( HasType FontCache r
     , HasType SDL.Renderer r
     , HasType SDL.Window r
     , HasType SpriteSheetCache r
     , MonadReader r m
     , MonadUnliftIO m
     )
  => Window -> m ()
render (Window { title, size, background, picture }) = do
  window <- view (the @SDL.Window)
  renderer <- view (the @SDL.Renderer)

  do
    let windowTitleVar = SDL.windowTitle window
    oldTitle <- SDL.get windowTitleVar
    when (oldTitle /= title) (windowTitleVar $=! title)

  do
    let newSize = round <$> V.toV2 size
    let windowSizeVar = SDL.windowSize window
    oldSize <- SDL.get windowSizeVar
    when (oldSize /= newSize) (windowSizeVar $=! newSize)

  do
    let newBackground = RGBA.toV4 background
    let backgroundVar = SDL.rendererDrawColor renderer
    oldBackground <- SDL.get backgroundVar
    when (oldBackground /= newBackground) (backgroundVar $=! newBackground)

  SDL.clear renderer
  Picture.render picture
  SDL.present renderer
