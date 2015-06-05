module Graphics.WebGL.Types where

import Control.Monad.Error.Trans (ErrorT ())
import Control.Monad.Eff (Eff ())
import Control.Monad.Reader.Trans (ReaderT ())
import Graphics.Canvas (Canvas ())

import qualified Graphics.WebGL.Raw.Enums as Enum
import qualified Graphics.WebGL.Raw.Types as Raw

type WebGL a = forall eff. ReaderT Raw.WebGLContext (ErrorT WebGLError (Eff (canvas :: Canvas | eff))) a

data WebGLError
  = ContextLost
  | NullValue String
  | ShaderError String
  | ErrorCode ErrorCode

-- wrapped GLenums

class ToWebGLEnum a where
  toWebglEnum :: a -> Number

class FromWebGLEnum a where
  fromWebglEnum :: Number -> a

data BufferType
  = DepthBuffer
  | ColorBuffer
  | StencilBuffer

instance toWebglEnumBufferType :: ToWebGLEnum BufferType where
  toWebglEnum DepthBuffer   = Enum.depthBufferBit
  toWebglEnum ColorBuffer   = Enum.colorBufferBit
  toWebglEnum StencilBuffer = Enum.stencilBufferBit

data ErrorCode
  = NoError
  | InvalidEnum
  | InvalidValue
  | InvalidOperation
  | OutOfMemory
  | UnknownError

instance fromWebglEnumErrorCode :: FromWebGLEnum ErrorCode where
  fromWebglEnum n
      | n == Enum.noError           = NoError
      | n == Enum.invalidEnum       = InvalidEnum
      | n == Enum.invalidValue      = InvalidValue
      | n == Enum.invalidOperation  = InvalidOperation
      | otherwise                   = UnknownError

instance showErrorCode :: Show ErrorCode where
  show NoError          = "no error"
  show InvalidEnum      = "invalid enum"
  show InvalidValue     = "invalid value"
  show InvalidOperation = "invalid operation"
  show OutOfMemory      = "out of memory"
  show _                = "unknown error"

data ProgramParam
  = DeleteStatus
  | LinkStatus
  | ValidateStatus
  | AttachedShaders
  | ActiveAttrs
  | ActiveUniforms

instance toWebglEnumProgramParam :: ToWebGLEnum ProgramParam where
  toWebglEnum DeleteStatus    = Enum.deleteStatus
  toWebglEnum LinkStatus      = Enum.linkStatus
  toWebglEnum ValidateStatus  = Enum.validateStatus
  toWebglEnum AttachedShaders = Enum.attachedShaders
  toWebglEnum ActiveAttrs     = Enum.activeAttributes
  toWebglEnum ActiveUniforms  = Enum.activeUniforms

data ShaderType
  = FragmentShader
  | VertexShader

instance toWebglEnumShader :: ToWebGLEnum ShaderType where
  toWebglEnum FragmentShader = Enum.fragmentShader
  toWebglEnum VertexShader   = Enum.vertexShader
