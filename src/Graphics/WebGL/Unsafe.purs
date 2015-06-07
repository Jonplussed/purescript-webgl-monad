module Graphics.WebGL.Unsafe where

foreign import unsafeAsNumber """
  function unsafeAsNumber(x) {
    return x;
  }
""" :: forall a. a -> Number
