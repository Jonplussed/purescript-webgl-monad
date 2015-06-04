module Graphics.WebGL.Methods where

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Reader.Trans (liftReaderT)

import qualified Graphics.WebGL.Raw as Raw

import Graphics.WebGL.Types

clear :: BufferType -> WebGL Unit
clear buffer = do
  ctx <- ask
  liftReaderT $ liftEff $ Raw.clear ctx $ toWebglEnum buffer

clearColor :: Number -> Number -> Number -> Number -> WebGL Unit
clearColor r g b a = do
  ctx <- ask
  liftReaderT $ liftEff $ Raw.clearColor ctx r g b a

getError :: WebGL Number
getError = do
  ctx <- ask
  liftReaderT $ liftEff $ Raw.getError ctx

isContextLost :: WebGL Boolean
isContextLost = do
  ctx <- ask
  liftReaderT $ liftEff $ Raw.isContextLost ctx
