module Gam.Internal.Window where

import Gam.Internal.Collage           (Collage)
import Gam.Internal.Config            (Config(..))
import Gam.Internal.FontCache         (FontCache)
import Gam.Internal.FrameCount        (FrameCount)
import Gam.Internal.Prelude
import Gam.Internal.RenderedTextCache (RenderedTextCache)
import Gam.Internal.RGBA              (RGBA)
import Gam.Internal.SpriteSheetCache  (SpriteSheetCache)

import qualified Gam.Internal.Collage as Collage
import qualified Gam.Internal.RGBA    as RGBA

import qualified Linear
import qualified SDL

data Window
  = Window
  { background :: RGBA
  , collage :: Collage
  } deriving stock (Generic)

new :: MonadIO m => Config -> m SDL.Window
new (Config { size, title }) =
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
     , HasType (IORef FrameCount) r
     , HasType RenderedTextCache r
     , HasType SpriteSheetCache r
     , HasType SDL.Renderer r
     , HasType SDL.Window r
     , MonadIO m
     , MonadReader r m
     )
  => Window
  -> m ()
render (Window { background, collage }) = do
  window <- view (the @SDL.Window)
  renderer <- view (the @SDL.Renderer)

  do
    let newBackground = RGBA.toV4 background
    let backgroundVar = SDL.rendererDrawColor renderer
    oldBackground <- SDL.get backgroundVar
    when (oldBackground /= newBackground) (backgroundVar $=! newBackground)

  SDL.clear renderer
  Collage.render collage
  SDL.present renderer
