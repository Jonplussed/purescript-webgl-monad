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
import Data.Function (Fn3 (..), runFn3)
import Data.Maybe (Maybe (..))
import Graphics.Canvas (Canvas ())
import Graphics.WebGL.Raw.Util (toMaybe)

import qualified Graphics.WebGL.Methods as GL
import qualified Graphics.WebGL.Raw.Types as Raw

import Graphics.WebGL.Types

class SetVertAttr a where
  setVertAttr :: Attribute a -> a -> WebGL Unit

instance setVertAttrNumber :: SetVertAttr Number where
  setVertAttr attr x = GL.vertexAttrib1f attr x

instance setVertAttrVec2 :: SetVertAttr Vec2 where
  setVertAttr attr (Vec2 x y) = GL.vertexAttrib2f attr x y

instance setVertAttrVec3 :: SetVertAttr Vec3 where
  setVertAttr attr (Vec3 x y z) = GL.vertexAttrib3f attr x y z

instance setVertAttrVec4 :: SetVertAttr Vec4 where
  setVertAttr attr (Vec4 x y z w) = GL.vertexAttrib4f attr x y z w

class SetUniform a where
  setUniform :: Uniform a -> a -> WebGL Unit

instance setUniformNumber :: SetUniform Number where
  setUniform unif x = GL.uniform1f unif x

instance setUniformVec2 :: SetUniform Vec2 where
  setUniform unif (Vec2 x y) = GL.uniform2f unif x y

instance setUniformVec3 :: SetUniform Vec3 where
  setUniform unif (Vec3 x y z) = GL.uniform3f unif x y z

instance setUniformVec4 :: SetUniform Vec4 where
  setUniform unif (Vec4 x y z w) = GL.uniform4f unif x y z w

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
    result <- liftEff $ getAttrBindings_ ctx prog
    case result of
      Just val -> return val
      Nothing -> throwError $ NullValue "getAttrBindings"

getUniformBindings :: forall bindings. WebGLProgram -> WebGL (Object bindings)
getUniformBindings prog = do
    ctx <- ask
    result <- liftEff $ getUniformBindings_ ctx prog
    case result of
      Just val -> return val
      Nothing -> throwError $ NullValue "getUniformBindings"

-- foreigns

foreign import getAttrBindingsImpl """
  function getAttrBindingsImpl(ctx, prog, wrapper) {
    return function () {
      var all, attr, count, loc;

      try {
        all = {};
        count = ctx.getProgramParameter(prog, ctx.ACTIVE_ATTRIBUTES);

        for (var i = 0; i < count; i++) {
          attr = ctx.getActiveAttrib(prog, i);
          loc = ctx.getAttribLocation(prog, attr.name);
          all[attr.name] = wrapper(loc);
        }

        return all;
      } catch(e) {
        return null;
      }
    };
  }
""" :: forall eff bindings a. Fn3 WebGLContext WebGLProgram (Number -> Attribute a) (Eff (canvas :: Canvas | eff) (Object bindings))

getAttrBindings_ :: forall eff bindings a. WebGLContext -> WebGLProgram -> Eff (canvas :: Canvas | eff) (Maybe (Object bindings))
getAttrBindings_ ctx prog = runFn3 getAttrBindingsImpl ctx prog Attribute >>= toMaybe >>> return

foreign import getUniformBindingsImpl """
  function getUniformBindingsImpl(ctx, prog, wrapper) {
    return function () {
      var all, unif, count, loc;

      try {
        all = {};
        count = ctx.getProgramParameter(prog, ctx.ACTIVE_UNIFORMS);

        for (var i = 0; i < count; i++) {
          unif = ctx.getActiveUniform(prog, i);
          loc = ctx.getUniformLocation(prog, unif.name);
          all[unif.name] = wrapper(loc);
        }

        return all;
      } catch(e) {
        return null;
      }
    };
  }
""" :: forall eff bindings a. Fn3 WebGLContext WebGLProgram (WebGLUniformLocation -> Uniform a) (Eff (canvas :: Canvas | eff) (Object bindings))

getUniformBindings_ :: forall eff bindings a. WebGLContext -> WebGLProgram -> Eff (canvas :: Canvas | eff) (Maybe (Object bindings))
getUniformBindings_ ctx prog = runFn3 getUniformBindingsImpl ctx prog Uniform >>= toMaybe >>> return
