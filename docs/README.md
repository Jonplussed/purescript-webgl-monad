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
attachShader :: WebGLProgram -> WebGLShader -> WebGL Unit
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
compileShader :: WebGLShader -> WebGL Unit
```


#### `createProgram`

``` purescript
createProgram :: WebGL WebGLProgram
```


#### `createShader`

``` purescript
createShader :: ShaderType -> WebGL WebGLShader
```


#### `drawArrays`

``` purescript
drawArrays :: DrawMode -> Number -> Number -> WebGL Unit
```


#### `getError`

``` purescript
getError :: WebGL Number
```


#### `getProgramParameter`

``` purescript
getProgramParameter :: forall a. WebGLProgram -> ProgramParam -> WebGL a
```


#### `isContextLost`

``` purescript
isContextLost :: WebGL Boolean
```


#### `linkProgram`

``` purescript
linkProgram :: WebGLProgram -> WebGL Unit
```


#### `shaderSource`

``` purescript
shaderSource :: WebGLShader -> String -> WebGL Unit
```



## Module Graphic.WebGL.Shader

#### `shaderLinkError`

``` purescript
shaderLinkError :: WebGLError
```

#### `useShaders`

``` purescript
useShaders :: String -> String -> WebGL Raw.WebGLProgram
```


## Module Graphics.WebGL.Types

#### `WebGL`

``` purescript
type WebGL a = forall eff. ReaderT Raw.WebGLContext (ErrorT WebGLError (Eff (canvas :: Canvas | eff))) a
```


#### `WebGLError`

``` purescript
data WebGLError
  = ContextLost 
  | ErrorCode ErrorCode
  | NullValue String
  | ShaderError String
```


#### `showWebGLError`

``` purescript
instance showWebGLError :: Show WebGLError
```


#### `WebGLContext`

``` purescript
type WebGLContext = Raw.WebGLContext
```

#### `WebGLContextAttributes`

``` purescript
type WebGLContextAttributes = Raw.WebGLContextAttributes
```


#### `WebGLProgram`

``` purescript
type WebGLProgram = Raw.WebGLProgram
```


#### `WebGLShader`

``` purescript
type WebGLShader = Raw.WebGLShader
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


#### `DrawMode`

``` purescript
data DrawMode
  = Points 
  | Lines 
  | LineLoop 
  | LineStrip 
  | Triangles 
  | TriangleStrip 
  | TriangleFan 
```


#### `toWebglEnumDrawMode`

``` purescript
instance toWebglEnumDrawMode :: ToWebGLEnum DrawMode
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