module Graphics.WebGL where

import Control.Monad (when)
import Control.Monad.Eff (Eff ())
import Control.Monad.Error.Class (throwError)
import Control.Monad.Error.Trans (runErrorT)
import Control.Monad.Reader.Trans (runReaderT)
import Data.Either (Either ())
import Data.Maybe (Maybe (..))
import Graphics.Canvas (Canvas (), CanvasElement (), getCanvasElementById)

import qualified Graphics.WebGL.Raw.Enums as Enum
import qualified Graphics.WebGL.Raw.Types as Raw

import Graphics.WebGL.Methods
import Graphics.WebGL.Shader
import Graphics.WebGL.Types

runWebgl :: forall eff a. WebGL a -> Raw.WebGLContext -> Eff (canvas :: Canvas | eff) (Either WebGLError a)
runWebgl f ctx = runErrorT $ runReaderT f ctx

runWebglWithShaders :: forall eff attrs uniforms a. (WebGLProgram -> Object attrs -> Object uniforms -> WebGL a) -> WebGLContext -> String -> String -> Eff (canvas :: Canvas | eff) (Either WebGLError a)
runWebglWithShaders f ctx vertSrc fragSrc = runWebgl (do
    prog <- compileShadersIntoProgram vertSrc fragSrc
    attr <- getAttrBindings prog
    unif <- getUniformBindings prog
    f prog attr unif) ctx

debug :: WebGL Unit
debug = do
    hasCtx <- not <$> isContextLost
    err <- fromWebglEnum <$> getError
    when (hasCtx && hasErr err) (throwError $ ErrorCode err)
  where
    hasErr NoError = false
    hasErr _       = true
