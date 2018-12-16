module Gam.Internal.Window where

import Gam.Internal.Prelude
import Gam.Internal.Render
import Gam.Internal.V (V(..))

import qualified Gam.Internal.V as V

import qualified SDL

data Window
  = Window
  { title :: Text
  , size  :: V Int
  } deriving stock (Generic)

new :: MonadIO m => Window -> m SDL.Window
new window =
  SDL.createWindow (window ^. the @"title") windowConfig
  where
    windowConfig :: SDL.WindowConfig
    windowConfig =
      SDL.defaultWindow
        { SDL.windowInitialSize =
            fromIntegral <$> V.toV2 (window ^. the @"size")
        }

render :: Window -> Render ()
render window = do
  env <- ask

  let
    sdlWindow :: SDL.Window
    sdlWindow =
      env ^. the @"window"

  do
    let newTitle = window ^. the @"title"
    oldTitle <- SDL.get (SDL.windowTitle sdlWindow)
    when (oldTitle /= newTitle)
      (SDL.windowTitle sdlWindow $=! newTitle)

  do
    let newSize = fromIntegral <$> V.toV2 (window ^. the @"size")
    oldSize <- SDL.get (SDL.windowSize sdlWindow)
    when (oldSize /= newSize)
      (SDL.windowSize sdlWindow $=! newSize)
