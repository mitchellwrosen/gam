module Gam.Main.Tea where

import Gam.Internal.Prelude
import Gam.Internal.Window  (Window)

import qualified Gam.Internal.Render as Render
import qualified Gam.Internal.Window as Window

import qualified SDL


main ::
     state
  -> (state -> Window)
  -> IO ()
main state render = do
  SDL.initialize [SDL.InitVideo]

  let
    window :: Window
    window =
      render state

  sdlWindow <-
    Window.new window

  renderer <-
    SDL.createRenderer sdlWindow (-1) SDL.RendererConfig
      { SDL.rendererType = SDL.AcceleratedVSyncRenderer
      , SDL.rendererTargetTexture = False
      }

  Render.run sdlWindow renderer (Window.render window)
