{-# LANGUAGE DataKinds, OverloadedLabels, StrictData, TypeOperators #-}

module Gam.Window
  ( Window
  , new
  ) where

import Gam.Prelude
import Gam.V

import Named

import qualified SDL


data Window
  = Window (IORef ()) SDL.Window SDL.Renderer

new ::
     MonadIO m
  => "title" :! Text
  -> "size"  :! V Int
  -> m Window
new (Arg title) (Arg (V sx sy)) = do
  window <- SDL.createWindow title windowConfig
  renderer <- SDL.createRenderer window (-1) rendererConfig

  let
    finalize :: IO ()
    finalize = do
      _ <- tryAny (SDL.destroyRenderer renderer)
      _ <- tryAny (SDL.destroyWindow window)
      pure ()

  aliveRef <- liftIO (newIORef ())
  _ <- liftIO (mkWeakIORef aliveRef finalize)

  pure (Window aliveRef window renderer)

  where
    windowConfig :: SDL.WindowConfig
    windowConfig =
      SDL.defaultWindow
        { SDL.windowInitialSize = SDL.V2 (fromIntegral sx) (fromIntegral sy)
        }

    rendererConfig :: SDL.RendererConfig
    rendererConfig =
      SDL.RendererConfig
        { SDL.rendererType = SDL.AcceleratedVSyncRenderer
        , SDL.rendererTargetTexture = False
        }
