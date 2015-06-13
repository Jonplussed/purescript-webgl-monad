# Module Documentation

## Module Graphics.WebGL

#### `runWebgl`

``` purescript
runWebgl :: forall eff a. WebGL a -> Raw.WebGLContext -> Eff (canvas :: Canvas | eff) (Either WebGLError a)
```


#### `runWebglWithShaders`

``` purescript
runWebglWithShaders :: forall eff attrs uniforms a. (WebGLProgram -> Object attrs -> Object uniforms -> WebGL a) -> WebGLContext -> String -> String -> Eff (canvas :: Canvas | eff) (Either WebGLError a)
```


#### `debug`

``` purescript
debug :: WebGL Unit
```



## Module Graphics.WebGL.Context

#### `defaultWebglContextAttrs`

``` purescript
defaultWebglContextAttrs :: WebGLContextAttributes
```


#### `getWebglContextWithAttrs`

``` purescript
getWebglContextWithAttrs :: forall eff. CanvasElement -> WebGLContextAttributes -> Eff (canvas :: Canvas | eff) (Maybe WebGLContext)
```


#### `getWebglContext`

``` purescript
getWebglContext :: forall eff. CanvasElement -> Eff (canvas :: Canvas | eff) (Maybe WebGLContext)
```


#### `canvasElement`

``` purescript
canvasElement :: WebGL CanvasElement
```

#### `drawingBufferHeight`

``` purescript
drawingBufferHeight :: WebGL Number
```


#### `drawingBufferWidth`

``` purescript
drawingBufferWidth :: WebGL Number
```



## Module Graphics.WebGL.Methods

#### `attachShader`

``` purescript
attachShader :: WebGLProgram -> WebGLShader -> WebGL Unit
```


#### `bindBuffer`

``` purescript
bindBuffer :: ArrayBufferType -> WebGLBuffer -> WebGL Unit
```


#### `bufferData`

``` purescript
bufferData :: ArrayBufferType -> BufferData -> BufferUsage -> WebGL Unit
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


#### `createBuffer`

``` purescript
createBuffer :: WebGL WebGLBuffer
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


#### `uniform1f`

``` purescript
uniform1f :: forall u. Uniform u -> Number -> WebGL Unit
```


#### `uniform1fv`

``` purescript
uniform1fv :: forall u. Uniform u -> Float32Array -> WebGL Unit
```


#### `uniform2f`

``` purescript
uniform2f :: forall u. Uniform u -> Number -> Number -> WebGL Unit
```


#### `uniform2fv`

``` purescript
uniform2fv :: forall u. Uniform u -> Float32Array -> WebGL Unit
```


#### `uniform3f`

``` purescript
uniform3f :: forall u. Uniform u -> Number -> Number -> Number -> WebGL Unit
```


#### `uniform3fv`

``` purescript
uniform3fv :: forall u. Uniform u -> Float32Array -> WebGL Unit
```


#### `uniform4f`

``` purescript
uniform4f :: forall u. Uniform u -> Number -> Number -> Number -> Number -> WebGL Unit
```


#### `uniform4fv`

``` purescript
uniform4fv :: forall u. Uniform u -> Float32Array -> WebGL Unit
```


#### `useProgram`

``` purescript
useProgram :: WebGLProgram -> WebGL Unit
```


#### `vertexAttrib1f`

``` purescript
vertexAttrib1f :: forall a. Attribute a -> Number -> WebGL Unit
```


#### `vertexAttrib1fv`

``` purescript
vertexAttrib1fv :: forall a. Attribute a -> Float32Array -> WebGL Unit
```


#### `vertexAttrib2f`

``` purescript
vertexAttrib2f :: forall a. Attribute a -> Number -> Number -> WebGL Unit
```


#### `vertexAttrib2fv`

``` purescript
vertexAttrib2fv :: forall a. Attribute a -> Float32Array -> WebGL Unit
```


#### `vertexAttrib3f`

``` purescript
vertexAttrib3f :: forall a. Attribute a -> Number -> Number -> Number -> WebGL Unit
```


#### `vertexAttrib3fv`

``` purescript
vertexAttrib3fv :: forall a. Attribute a -> Float32Array -> WebGL Unit
```


#### `vertexAttrib4f`

``` purescript
vertexAttrib4f :: forall a. Attribute a -> Number -> Number -> Number -> Number -> WebGL Unit
```


#### `vertexAttrib4fv`

``` purescript
vertexAttrib4fv :: forall a. Attribute a -> Float32Array -> WebGL Unit
```


#### `vertexAttribPointer`

``` purescript
vertexAttribPointer :: forall a. Attribute a -> Number -> DataType -> Boolean -> Number -> Number -> WebGL Unit
```



## Module Graphics.WebGL.Shader

#### `setVertAttrNumber`

``` purescript
instance setVertAttrNumber :: SetVertAttr Number
```


#### `setVertAttrVec2`

``` purescript
instance setVertAttrVec2 :: SetVertAttr Vec2
```


#### `setVertAttrVec3`

``` purescript
instance setVertAttrVec3 :: SetVertAttr Vec3
```


#### `setVertAttrVec4`

``` purescript
instance setVertAttrVec4 :: SetVertAttr Vec4
```


#### `setUniformNumber`

``` purescript
instance setUniformNumber :: SetUniform Number
```


#### `setUniformVec2`

``` purescript
instance setUniformVec2 :: SetUniform Vec2
```


#### `setUniformVec3`

``` purescript
instance setUniformVec3 :: SetUniform Vec3
```


#### `setUniformVec4`

``` purescript
instance setUniformVec4 :: SetUniform Vec4
```


#### `compileShadersIntoProgram`

``` purescript
compileShadersIntoProgram :: String -> String -> WebGL WebGLProgram
```

#### `addShaderToProgram`

``` purescript
addShaderToProgram :: WebGLProgram -> ShaderType -> String -> WebGL Unit
```


#### `getAttrBindings`

``` purescript
getAttrBindings :: forall bindings. WebGLProgram -> WebGL (Object bindings)
```


#### `getUniformBindings`

``` purescript
getUniformBindings :: forall bindings. WebGLProgram -> WebGL (Object bindings)
```



## Module Graphics.WebGL.Types

#### `WebGLT`

``` purescript
type WebGLT eff a = ReaderT Raw.WebGLContext (ErrorT WebGLError eff) a
```


#### `WebGL`

``` purescript
type WebGL a = forall eff. WebGLT (Eff (canvas :: Canvas | eff)) a
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


#### `WebGLBuffer`

``` purescript
type WebGLBuffer = Raw.WebGLBuffer
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


#### `WebGLUniformLocation`

``` purescript
type WebGLUniformLocation = Raw.WebGLUniformLocation
```


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


#### `Attribute`

``` purescript
newtype Attribute a
  = Attribute Number
```

#### `Uniform`

``` purescript
newtype Uniform a
  = Uniform WebGLUniformLocation
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


#### `ArrayBufferType`

``` purescript
data ArrayBufferType
  = ArrayBuffer 
  | ElementArrayBuffer 
```


#### `toWebglEnumArrayBufferType`

``` purescript
instance toWebglEnumArrayBufferType :: ToWebGLEnum ArrayBufferType
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


#### `BufferUsage`

``` purescript
data BufferUsage
  = DynamicDraw 
  | StaticDraw 
  | StreamDraw 
```


#### `toWebglEnumBufferUsage`

``` purescript
instance toWebglEnumBufferUsage :: ToWebGLEnum BufferUsage
```


#### `DataType`

``` purescript
data DataType
  = Byte 
  | UnsignedByte 
  | Short 
  | UnsignedShort 
  | Int 
  | UnsignedInt 
  | Float 
```


#### `toWebglEnumDataType`

``` purescript
instance toWebglEnumDataType :: ToWebGLEnum DataType
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


#### `BufferData`

``` purescript
data BufferData
  = DataSize Number
  | DataSource Float32Array
```


## Module Graphics.WebGL.Unsafe

#### `unsafeCoerce`

``` purescript
unsafeCoerce :: forall a b. a -> b
```