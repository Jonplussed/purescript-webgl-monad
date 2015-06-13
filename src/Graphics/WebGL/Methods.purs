module Graphics.WebGL.Methods where

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Error.Class (throwError)
import Data.ArrayBuffer.Types (Float32Array ())
import Data.Maybe (Maybe (..))

import qualified Graphics.WebGL.Raw as Raw

import Graphics.WebGL.Types

attachShader :: WebGLProgram -> WebGLShader -> WebGL Unit
attachShader prog shader = do
    ctx <- ask
    liftEff $ Raw.attachShader ctx prog shader

bindBuffer :: ArrayBufferType -> WebGLBuffer -> WebGL Unit
bindBuffer btype buffer = do
    ctx <- ask
    liftEff $ Raw.bindBuffer ctx (toWebglEnum btype) buffer

bufferData :: ArrayBufferType -> BufferData -> BufferUsage -> WebGL Unit
bufferData btype datatype usage = do
    ctx <- ask
    liftEff $ case datatype of
      (DataSource ns) -> Raw.bufferData ctx (toWebglEnum btype) ns (toWebglEnum usage)
      (DataSize n)    -> Raw.bufferData_ ctx (toWebglEnum btype) n (toWebglEnum usage)

clear :: BufferType -> WebGL Unit
clear buffer = do
    ctx <- ask
    liftEff $ Raw.clear ctx $ toWebglEnum buffer

clearColor :: Number -> Number -> Number -> Number -> WebGL Unit
clearColor r g b a = do
    ctx <- ask
    liftEff $ Raw.clearColor ctx r g b a

compileShader :: WebGLShader -> WebGL Unit
compileShader shader = do
    ctx <- ask
    liftEff $ Raw.compileShader ctx shader

createBuffer :: WebGL WebGLBuffer
createBuffer = do
    result <- ask >>= Raw.createBuffer >>> liftEff
    case result of
      Just buffer -> return buffer
      Nothing -> throwError $ NullValue "createBuffer"

createProgram :: WebGL WebGLProgram
createProgram = do
    ctx <- ask
    result <- liftEff $ Raw.createProgram ctx
    case result of
      Just prog -> return prog
      Nothing -> throwError $ NullValue "createProgram"

createShader :: ShaderType -> WebGL WebGLShader
createShader stype = do
    ctx <- ask
    result <- liftEff $ Raw.createShader ctx $ toWebglEnum stype
    case result of
      Just shader -> return shader
      Nothing -> throwError $ NullValue "createShader"

drawArrays :: DrawMode -> Number -> Number -> WebGL Unit
drawArrays mode first count = do
    ctx <- ask
    liftEff $ Raw.drawArrays ctx (toWebglEnum mode) first count

getError :: WebGL Number
getError = ask >>= Raw.getError >>> liftEff

getProgramParameter :: forall a. WebGLProgram -> ProgramParam -> WebGL a
getProgramParameter prog param = do
    ctx <- ask
    result <- liftEff $ Raw.getProgramParameter ctx prog $ toWebglEnum param
    case result of
      Just val -> return val
      Nothing -> throwError $ NullValue "getProgramParameter"

isContextLost :: WebGL Boolean
isContextLost = ask >>= Raw.isContextLost >>> liftEff

linkProgram :: WebGLProgram -> WebGL Unit
linkProgram prog = do
    ctx <- ask
    liftEff $ Raw.linkProgram ctx prog

shaderSource :: WebGLShader -> String -> WebGL Unit
shaderSource shader src = do
    ctx <- ask
    liftEff $ Raw.shaderSource ctx shader src

uniform1f :: forall a. Uniform a -> Number -> WebGL Unit
uniform1f (Uniform attr) x = do
    ctx <- ask
    liftEff $ Raw.uniform1f ctx attr x

uniform1fv :: forall a. Uniform a -> Float32Array -> WebGL Unit
uniform1fv (Uniform attr) xs = do
    ctx <- ask
    liftEff $ Raw.uniform1fv_ ctx attr xs

uniform2f :: forall a. Uniform a -> Number -> Number -> WebGL Unit
uniform2f (Uniform attr) x y = do
    ctx <- ask
    liftEff $ Raw.uniform2f ctx attr x y

uniform2fv :: forall a. Uniform a -> Float32Array -> WebGL Unit
uniform2fv (Uniform attr) xs = do
    ctx <- ask
    liftEff $ Raw.uniform2fv_ ctx attr xs

uniform3f :: forall a. Uniform a -> Number -> Number -> Number -> WebGL Unit
uniform3f (Uniform attr) x y z = do
    ctx <- ask
    liftEff $ Raw.uniform3f ctx attr x y z

uniform3fv :: forall a. Uniform a -> Float32Array -> WebGL Unit
uniform3fv (Uniform attr) xs = do
    ctx <- ask
    liftEff $ Raw.uniform3fv_ ctx attr xs

uniform4f :: forall a. Uniform a -> Number -> Number -> Number -> Number -> WebGL Unit
uniform4f (Uniform attr) x y z w = do
    ctx <- ask
    liftEff $ Raw.uniform4f ctx attr x y z w

uniform4fv :: forall a. Uniform a -> Float32Array -> WebGL Unit
uniform4fv (Uniform attr) xs = do
    ctx <- ask
    liftEff $ Raw.uniform4fv_ ctx attr xs

useProgram :: WebGLProgram -> WebGL Unit
useProgram prog = do
    ctx <- ask
    liftEff $ Raw.useProgram ctx prog

vertexAttrib1f :: forall a. Attribute a -> Number -> WebGL Unit
vertexAttrib1f (Attribute a) x = do
    ctx <- ask
    liftEff $ Raw.vertexAttrib1f ctx a x

vertexAttrib1fv :: forall a. Attribute a -> Float32Array -> WebGL Unit
vertexAttrib1fv (Attribute a) xs = do
    ctx <- ask
    liftEff $ Raw.vertexAttrib1fv_ ctx a xs

vertexAttrib2f :: forall a. Attribute a -> Number -> Number -> WebGL Unit
vertexAttrib2f (Attribute a) x y = do
    ctx <- ask
    liftEff $ Raw.vertexAttrib2f ctx a x y

vertexAttrib2fv :: forall a. Attribute a -> Float32Array -> WebGL Unit
vertexAttrib2fv (Attribute a) xs = do
    ctx <- ask
    liftEff $ Raw.vertexAttrib2fv_ ctx a xs

vertexAttrib3f :: forall a. Attribute a -> Number -> Number -> Number -> WebGL Unit
vertexAttrib3f (Attribute a) x y z = do
    ctx <- ask
    liftEff $ Raw.vertexAttrib3f ctx a x y z

vertexAttrib3fv :: forall a. Attribute a -> Float32Array -> WebGL Unit
vertexAttrib3fv (Attribute a) xs = do
    ctx <- ask
    liftEff $ Raw.vertexAttrib3fv_ ctx a xs

vertexAttrib4f :: forall a. Attribute a -> Number -> Number -> Number -> Number -> WebGL Unit
vertexAttrib4f (Attribute a) x y z w = do
    ctx <- ask
    liftEff $ Raw.vertexAttrib4f ctx a x y z w

vertexAttrib4fv :: forall a. Attribute a -> Float32Array -> WebGL Unit
vertexAttrib4fv (Attribute a) xs = do
    ctx <- ask
    liftEff $ Raw.vertexAttrib4fv_ ctx a xs
