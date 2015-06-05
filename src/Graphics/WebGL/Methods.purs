module Graphics.WebGL.Methods where

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Reader.Trans (liftReaderT)
import Control.Monad.Error.Class (throwError)
import Data.Maybe (Maybe (..))

import qualified Graphics.WebGL.Raw       as Raw
import qualified Graphics.WebGL.Raw.Types as Raw

import Graphics.WebGL.Types

attachShader :: Raw.WebGLProgram -> Raw.WebGLShader -> WebGL Unit
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

compileShader :: Raw.WebGLShader -> WebGL Unit
compileShader shader = do
    ctx <- ask
    liftEff $ Raw.compileShader ctx shader

createProgram :: WebGL Raw.WebGLProgram
createProgram = do
    ctx <- ask
    result <- liftEff $ Raw.createProgram ctx
    case result of
      Just prog -> return prog
      Nothing -> throwError $ NullValue "createProgram"

createShader :: ShaderType -> WebGL Raw.WebGLShader
createShader stype = do
    ctx <- ask
    result <- liftEff $ Raw.createShader ctx $ toWebglEnum stype
    case result of
      Just shader -> return shader
      Nothing -> throwError $ NullValue "createShader"

getError :: WebGL Number
getError = ask >>= Raw.getError >>> liftEff

getProgramParameter :: forall a. Raw.WebGLProgram -> ProgramParam -> WebGL a
getProgramParameter prog param = do
    ctx <- ask
    result <- liftEff $ Raw.getProgramParameter ctx prog $ toWebglEnum param
    case result of
      Just val -> return val
      Nothing -> throwError $ NullValue "getProgramParameter"

isContextLost :: WebGL Boolean
isContextLost = ask >>= Raw.isContextLost >>> liftEff

linkProgram :: Raw.WebGLProgram -> WebGL Unit
linkProgram prog = do
    ctx <- ask
    liftEff $ Raw.linkProgram ctx prog

shaderSource :: Raw.WebGLShader -> String -> WebGL Unit
shaderSource shader src = do
    ctx <- ask
    liftEff $ Raw.shaderSource ctx shader src

