module Graphics.WebGL.Shader
( addShaderToProgram
, compileShadersIntoProgram
, getAttrBindings
, getUniformBindings
) where

import Control.Monad (when, foldM)
import Control.Monad.Eff (Eff ())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader.Class (ask)
import Data.Function (Fn2 (..), runFn2)
import Data.Maybe (Maybe (..))
import Graphics.Canvas (Canvas ())
import Graphics.WebGL.Raw.Util (toMaybe)

import qualified Graphics.WebGL.Methods as GL
import qualified Graphics.WebGL.Raw.Types as Raw

import Graphics.WebGL.Types

-- constants

shaderLinkError :: WebGLError
shaderLinkError = ShaderError "could not link shaders prog"

-- public functions

addShaderToProgram :: WebGLProgram -> ShaderType -> String -> WebGL Unit
addShaderToProgram prog stype src = do
    shader <- GL.createShader stype
    GL.shaderSource shader src
    GL.compileShader shader
    GL.attachShader prog shader

compileShadersIntoProgram :: String -> String -> WebGL WebGLProgram
compileShadersIntoProgram vertSrc fragSrc = do
    prog <- GL.createProgram
    addShaderToProgram prog VertexShader vertSrc
    addShaderToProgram prog FragmentShader fragSrc
    GL.linkProgram prog

    isLinked <- GL.getProgramParameter prog LinkStatus
    when (not isLinked) (throwError shaderLinkError)

    GL.useProgram prog
    return prog

getAttrBindings :: forall bindings. WebGLProgram -> WebGL (Object bindings)
getAttrBindings prog = do
    ctx <- ask
    result <- liftEff $ getAttrBindings' ctx prog
    case result of
      Just val -> return val
      Nothing -> throwError $ NullValue "getAttrBindings"

getUniformBindings :: forall bindings. WebGLProgram -> WebGL (Object bindings)
getUniformBindings prog = do
    ctx <- ask
    result <- liftEff $ getUniformBindings' ctx prog
    case result of
      Just val -> return val
      Nothing -> throwError $ NullValue "getUniformBindings"

-- foreigns

foreign import getAttrBindingsImpl """
  function getAttrBindingsImpl(ctx, prog) {
    var attr, attrs, count, loc;

    try {
      attrs = {};
      count = ctx.getProgramParameter(prog, ctx.ACTIVE_ATTRIBUTES);

      for (var i = 0; i < count; i++) {
        attr = ctx.getActiveAttrib(prog, i);
        loc = ctx.getAttribLocation(prog, attr.name);
        ctx.enableVertexAttribArray(loc);
        attrs[attr.name] = attr;
      }

      return attrs;
    } catch(e) {
      return null;
    }
  }
""" :: forall eff bindings. Fn2 WebGLContext WebGLProgram (Eff (canvas :: Canvas | eff) (Object bindings))

getAttrBindings' :: forall eff bindings. WebGLContext -> WebGLProgram -> Eff (canvas :: Canvas | eff) (Maybe (Object bindings))
getAttrBindings' ctx prog = runFn2 getAttrBindingsImpl ctx prog >>= toMaybe >>> return

foreign import getUniformBindingsImpl """
  function getUniformBindingsImpl(ctx, prog) {
    var unif, unifs, count;

    try {
      unifs = {};
      count = ctx.getProgramParameter(pro, ctx.ACTIVE_UNIFORMS);

      for (var i = 0; i < count; i++) {
        unif = ctx.getActiveUniform(prog, i);
        unifs[unif.name] = unifs;
      }

      return unifs;
    } catch(e) {
      return null;
    }
  }
""" :: forall eff bindings. Fn2 WebGLContext WebGLProgram (Eff (canvas :: Canvas | eff) (Object bindings))

getUniformBindings' :: forall eff bindings. WebGLContext -> WebGLProgram -> Eff (canvas :: Canvas | eff) (Maybe (Object bindings))
getUniformBindings' ctx prog = runFn2 getUniformBindingsImpl ctx prog >>= toMaybe >>> return
