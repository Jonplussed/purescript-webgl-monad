module Graphics.WebGL.Context
( defaultWebglContextAttributes
, getWebglContext
, getWebglContextWithAttrs
) where

import Control.Monad.Eff (Eff ())
import Data.Function (Fn4 (..), runFn4)
import Data.Maybe (Maybe (..))
import Graphics.Canvas (Canvas (), CanvasElement ())

import qualified Graphics.WebGL.Raw.Types as Raw

import Graphics.WebGL.Types

defaultWebglContextAttributes :: Raw.WebGLContextAttributes
defaultWebglContextAttributes =
  { alpha:                            true
  , depth:                            true
  , stencil:                          false
  , antialias:                        true
  , premultipliedAlpha:               true
  , preserveDrawingBuffer:            false
  , preferLowPowerToHighPerformance:  false
  , failIfMajorPerformanceCaveat:     false
  }

getWebglContextWithAttrs :: CanvasElement -> Raw.WebGLContextAttributes -> Eff (canvas :: Canvas) (Maybe Raw.WebGLContext)
getWebglContextWithAttrs canvas attrs = runFn4 getWebglContextWithAttrsImpl canvas attrs Just Nothing

getWebglContext :: CanvasElement -> Eff (canvas :: Canvas) (Maybe Raw.WebGLContext)
getWebglContext canvas = getWebglContextWithAttrs canvas defaultWebglContextAttributes

-- foreigns

foreign import getWebglContextWithAttrsImpl """
  function getWebglContextImpl(canvas, attrs, Just, Nothing) {
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
""" :: forall maybe. Fn4 CanvasElement Raw.WebGLContextAttributes (Raw.WebGLContext -> maybe) maybe (Eff (canvas :: Canvas) (Maybe Raw.WebGLContext))
