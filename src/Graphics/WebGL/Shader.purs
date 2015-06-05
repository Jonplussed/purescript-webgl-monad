module Graphics.WebGL.Shader where

import Control.Monad.Error.Class (throwError)

import qualified Graphics.WebGL.Methods as GL
import qualified Graphics.WebGL.Raw.Types as Raw

import Graphics.WebGL.Types

-- constants

shaderLinkError :: WebGLError
shaderLinkError = ShaderError "could not link shaders program"

-- public functions

useShaders :: String -> String -> WebGL Raw.WebGLProgram
useShaders vertStr fragStr = do
    program <- GL.createProgram

    vertShader <- GL.createShader VertexShader
    GL.shaderSource vertShader vertStr
    GL.compileShader vertShader
    GL.attachShader program vertShader

    fragShader <- GL.createShader FragmentShader
    GL.shaderSource fragShader fragStr
    GL.compileShader fragShader
    GL.attachShader program fragShader

    GL.linkProgram program
    isLinked <- GL.getProgramParameter program LinkStatus

    case isLinked of
      true -> return program
      _    -> throwError shaderLinkError
