module Graphics.WebGL.Methods where

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Error.Class (throwError)
import Data.Maybe (Maybe (..))

import qualified Graphics.WebGL.Raw as Raw

import Graphics.WebGL.Types

attachShader :: WebGLProgram -> WebGLShader -> WebGL Unit
attachShader prog shader = do
    ctx <- ask
    liftEff $ Raw.attachShader ctx prog shader

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

vertexAttrib1f :: forall a. Attribute a -> Number -> WebGL Unit
vertexAttrib1f (Attribute attr) x = do
    ctx <- ask
    liftEff $ Raw.vertexAttrib1f ctx attr x

vertexAttrib3f :: forall a. Attribute a -> Number -> Number -> Number -> WebGL Unit
vertexAttrib3f (Attribute attr) x y z = do
    ctx <- ask
    liftEff $ Raw.vertexAttrib3f ctx attr x y z

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

useProgram :: WebGLProgram -> WebGL Unit
useProgram prog = do
    ctx <- ask
    liftEff $ Raw.useProgram ctx prog
