module Graphics.WebGL.Shader
( programShaders
) where

import Control.Monad (when)
import Control.Monad.Error.Class (throwError)

import qualified Graphics.WebGL.Methods as GL
import qualified Graphics.WebGL.Raw.Types as Raw

import Graphics.WebGL.Types

-- constants

shaderLinkError :: WebGLError
shaderLinkError = ShaderError "could not link shaders prog"

-- public functions

programShaders :: String -> String -> WebGL Raw.WebGLProgram
programShaders vertSrc fragSrc = do
    prog <- GL.createProgram
    initShader prog VertexShader vertSrc
    initShader prog FragmentShader fragSrc
    GL.linkProgram prog

    isLinked <- GL.getProgramParameter prog LinkStatus
    when (not isLinked) (throwError shaderLinkError)

    GL.useProgram prog
    return prog

initShader :: WebGLProgram -> ShaderType -> String -> WebGL Unit
initShader prog stype src = do
    shader <- GL.createShader stype
    GL.shaderSource shader src
    GL.compileShader shader
    GL.attachShader prog shader
