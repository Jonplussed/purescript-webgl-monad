module Graphics.WebGL.Types where

import Control.Monad.Error.Trans (ErrorT ())
import Control.Monad.Eff (Eff ())
import Control.Monad.Reader.Trans (ReaderT ())
import Graphics.Canvas (Canvas ())

import qualified Graphics.WebGL.Raw.Enums as Enum
import qualified Graphics.WebGL.Raw.Types as Raw

type WebGL a = ReaderT Raw.WebGLContext (ErrorT ErrorCode (Eff (canvas :: Canvas))) a

class ToWebGLEnum a where
  toWebglEnum :: a -> Number

class FromWebGLEnum a where
  fromWebglEnum :: Number -> a

data BufferType
  = DepthBuffer
  | ColorBuffer
  | StencilBuffer

instance toWebglEnumBufferType :: ToWebGLEnum BufferType where
  toWebglEnum DepthBuffer = Enum.depthBufferBit
  toWebglEnum ColorBuffer = Enum.colorBufferBit
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
