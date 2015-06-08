module Graphics.WebGL.Unsafe where

import Control.Monad.Eff (Eff ())

foreign import unsafeCoerce """
  function unsafeCoerce(x) {
    return x;
  }
""" :: forall a b. a -> b
