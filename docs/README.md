# Module Documentation

## Module Graphics.WebGL

#### `debug`

``` purescript
debug :: WebGL Unit
```


#### `runWebgl`

``` purescript
runWebgl :: forall eff a. WebGL a -> Raw.WebGLContext -> Eff (canvas :: Canvas | eff) (Either ErrorCode a)
```



## Module Graphics.WebGL.Context

#### `defaultWebglContextAttributes`

``` purescript
defaultWebglContextAttributes :: Raw.WebGLContextAttributes
```


#### `getWebglContextWithAttrs`

``` purescript
getWebglContextWithAttrs :: forall eff. CanvasElement -> Raw.WebGLContextAttributes -> Eff (canvas :: Canvas | eff) (Maybe Raw.WebGLContext)
```


#### `getWebglContext`

``` purescript
getWebglContext :: forall eff. CanvasElement -> Eff (canvas :: Canvas | eff) (Maybe Raw.WebGLContext)
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
type WebGL a = forall eff. ReaderT Raw.WebGLContext (ErrorT ErrorCode (Eff (canvas :: Canvas | eff))) a
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


#### `showErrorCode`

``` purescript
instance showErrorCode :: Show ErrorCode
```