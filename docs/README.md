# Module Documentation

## Module Graphics.WebGL

#### `debug`

``` purescript
debug :: WebGL Unit
```


#### `runWebgl`

``` purescript
runWebgl :: forall eff a. WebGL a -> Raw.WebGLContext -> Eff (canvas :: Canvas | eff) (Either WebGLError a)
```



## Module Graphics.WebGL.Context

#### `defaultWebglContextAttrs`

``` purescript
defaultWebglContextAttrs :: Raw.WebGLContextAttributes
```


#### `getWebglContextWithAttrs`

``` purescript
getWebglContextWithAttrs :: forall eff. CanvasElement -> Raw.WebGLContextAttributes -> Eff (canvas :: Canvas | eff) (Maybe Raw.WebGLContext)
```


#### `getWebglContext`

``` purescript
getWebglContext :: forall eff. CanvasElement -> Eff (canvas :: Canvas | eff) (Maybe Raw.WebGLContext)
```



## Module Graphics.WebGL.Math

#### `Vec2`

``` purescript
data Vec2
  = Vec2 Number Number
```


#### `Vec3`

``` purescript
data Vec3
  = Vec3 Number Number Number
```


#### `Vec4`

``` purescript
data Vec4
  = Vec4 Number Number Number Number
```


#### `Mat2`

``` purescript
data Mat2
  = Mat2 Number Number Number Number
```


#### `Mat3`

``` purescript
data Mat3
  = Mat3 Number Number Number Number Number Number Number Number Number
```


#### `Mat4`

``` purescript
data Mat4
  = Mat4 Number Number Number Number Number Number Number Number Number Number Number Number Number Number Number Number
```



## Module Graphics.WebGL.Methods

#### `attachShader`

``` purescript
attachShader :: Raw.WebGLProgram -> Raw.WebGLShader -> WebGL Unit
```


#### `clear`

``` purescript
clear :: BufferType -> WebGL Unit
```


#### `clearColor`

``` purescript
clearColor :: Number -> Number -> Number -> Number -> WebGL Unit
```


#### `compileShader`

``` purescript
compileShader :: Raw.WebGLShader -> WebGL Unit
```


#### `createProgram`

``` purescript
createProgram :: WebGL Raw.WebGLProgram
```


#### `createShader`

``` purescript
createShader :: ShaderType -> WebGL Raw.WebGLShader
```


#### `getError`

``` purescript
getError :: WebGL Number
```


#### `getProgramParameter`

``` purescript
getProgramParameter :: forall a. Raw.WebGLProgram -> ProgramParam -> WebGL a
```


#### `isContextLost`

``` purescript
isContextLost :: WebGL Boolean
```


#### `linkProgram`

``` purescript
linkProgram :: Raw.WebGLProgram -> WebGL Unit
```


#### `shaderSource`

``` purescript
shaderSource :: Raw.WebGLShader -> String -> WebGL Unit
```



## Module Graphic.WebGL.Shader


## Module Graphics.WebGL.Types

#### `WebGL`

``` purescript
type WebGL a = forall eff. ReaderT Raw.WebGLContext (ErrorT WebGLError (Eff (canvas :: Canvas | eff))) a
```


#### `WebGLError`

``` purescript
data WebGLError
  = ContextLost 
  | NullValue String
  | ErrorCode ErrorCode
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


#### `ProgramParam`

``` purescript
data ProgramParam
  = DeleteStatus 
  | LinkStatus 
  | ValidateStatus 
  | AttachedShaders 
  | ActiveAttrs 
  | ActiveUniforms 
```


#### `toWebglEnumProgramParam`

``` purescript
instance toWebglEnumProgramParam :: ToWebGLEnum ProgramParam
```


#### `ShaderType`

``` purescript
data ShaderType
  = FragmentShader 
  | VertexShader 
```


#### `toWebglEnumShader`

``` purescript
instance toWebglEnumShader :: ToWebGLEnum ShaderType
```