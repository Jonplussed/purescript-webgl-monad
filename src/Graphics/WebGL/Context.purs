module Graphics.WebGL.Context
( defaultWebglContextAttrs
, getWebglContext
, getWebglContextWithAttrs
) where

import Control.Monad.Eff (Eff ())
import Data.Function (Fn4 (..), runFn4)
import Data.Maybe (Maybe (..))
import Graphics.Canvas (Canvas (), CanvasElement ())

import qualified Graphics.WebGL.Raw.Types as Raw

import Graphics.WebGL.Types

defaultWebglContextAttrs :: Raw.WebGLContextAttributes
defaultWebglContextAttrs =
  { alpha:                            true
  , depth:                            true
  , stencil:                          false
  , antialias:                        true
  , premultipliedAlpha:               true
  , preserveDrawingBuffer:            false
  , preferLowPowerToHighPerformance:  false
  , failIfMajorPerformanceCaveat:     false
  }

getWebglContextWithAttrs :: forall eff. CanvasElement -> Raw.WebGLContextAttributes -> Eff (canvas :: Canvas | eff) (Maybe Raw.WebGLContext)
getWebglContextWithAttrs canvas attrs = runFn4 getWebglContextWithAttrsImpl canvas attrs Just Nothing

getWebglContext :: forall eff. CanvasElement -> Eff (canvas :: Canvas | eff) (Maybe Raw.WebGLContext)
getWebglContext canvas = getWebglContextWithAttrs canvas defaultWebglContextAttrs

-- foreigns

foreign import getWebglContextWithAttrsImpl """
  function getWebglContextWithAttrsImpl(canvas, attrs, Just, Nothing) {
    return function () {
      try {
        return Just(
          canvas.getContext('webgl', attrs) || canvas.getContext('experimental-webgl', attrs)
        );
      } catch(err) {
        return Nothing;
      };
    }
  }
""" :: forall eff maybe. Fn4 CanvasElement Raw.WebGLContextAttributes (Raw.WebGLContext -> maybe) maybe (Eff (canvas :: Canvas | eff) (Maybe Raw.WebGLContext))
