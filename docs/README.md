# Module Documentation

## Module Graphics.WebGL

#### `debug`

``` purescript
debug :: WebGL Unit
```


#### `runWebgl`

``` purescript
runWebgl :: forall a. WebGL a -> Raw.WebGLContext -> Eff (canvas :: Canvas) (Either ErrorCode a)
```



## Module Graphics.WebGL.Context

#### `defaultWebglContextAttributes`

``` purescript
defaultWebglContextAttributes :: Raw.WebGLContextAttributes
```


#### `getWebglContextWithAttrs`

``` purescript
getWebglContextWithAttrs :: CanvasElement -> Raw.WebGLContextAttributes -> Eff (canvas :: Canvas) (Maybe Raw.WebGLContext)
```


#### `getWebglContext`

``` purescript
getWebglContext :: CanvasElement -> Eff (canvas :: Canvas) (Maybe Raw.WebGLContext)
```



## Module Graphics.WebGL.Methods

#### `clear`

``` purescript
clear :: BufferType -> WebGL Unit
```


#### `clearColor`

``` purescript
clearColor :: Number -> Number -> Number -> Number -> WebGL Unit
```


#### `getError`

``` purescript
getError :: WebGL Number
```


#### `isContextLost`

``` purescript
isContextLost :: WebGL Boolean
```



## Module Graphics.WebGL.Types

#### `WebGL`

``` purescript
type WebGL a = ReaderT Raw.WebGLContext (ErrorT ErrorCode (Eff (canvas :: Canvas))) a
```


#### `ToWebGLEnum`

``` purescript
class ToWebGLEnum a where
  toWebglEnum :: a -> Number
```


#### `FromWebGLEnum`

``` purescript
class FromWebGLEnum a where
  fromWebglEnum :: Number -> a
```


#### `BufferType`

``` purescript
data BufferType
  = DepthBuffer 
  | ColorBuffer 
  | StencilBuffer 
```


#### `toWebglEnumBufferType`

``` purescript
instance toWebglEnumBufferType :: ToWebGLEnum BufferType
```


#### `ErrorCode`

``` purescript
data ErrorCode
  = NoError 
  | InvalidEnum 
  | InvalidValue 
  | InvalidOperation 
  | OutOfMemory 
  | UnknownError 
```


#### `fromWebglEnumErrorCode`

``` purescript
instance fromWebglEnumErrorCode :: FromWebGLEnum ErrorCode
```