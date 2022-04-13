#lang racket
(provide src)
(define src (open-input-string "#lang racket/base

(require ffi/unsafe raylib/support)

(provide (all-defined-out))

;; Vector2, 2 components
(define-cstruct _Vector2
  ([x _float] ; Vector x component
   [y _float] ; Vector y component
   ))

;; Vector3, 3 components
(define-cstruct _Vector3
  ([x _float] ; Vector x component
   [y _float] ; Vector y component
   [z _float] ; Vector z component
   ))

;; Vector4, 4 components
(define-cstruct _Vector4
  ([x _float] ; Vector x component
   [y _float] ; Vector y component
   [z _float] ; Vector z component
   [w _float] ; Vector w component
   ))

(define _Quaternion _Vector4)

;; Matrix, 4x4 components, column major, OpenGL style, right handed
(define-cstruct _Matrix
  ([m0 _float] [m4 _float] [m8 _float] [m12 _float] ; Matrix first row (4 components)
   [m1 _float] [m5 _float] [m9 _float] [m13 _float] ; Matrix second row (4 components)
   [m2 _float] [m6 _float] [m10 _float] [m14 _float] ; Matrix third row (4 components)
   [m3 _float] [m7 _float] [m11 _float] [m15 _float] ; Matrix fourth row (4 components)
   ))

;; Color, 4 components, R8G8B8A8 (32bit)
(define-cstruct _Color
  ([r _ubyte] ; Color red value
   [g _ubyte] ; Color green value
   [b _ubyte] ; Color blue value
   [a _ubyte] ; Color alpha value
   ))

;; Rectangle, 4 components
(define-cstruct _Rectangle
  ([x _float] ; Rectangle top-left corner position x
   [y _float] ; Rectangle top-left corner position y
   [width _float] ; Rectangle width
   [height _float] ; Rectangle height
   ))

;; Image, pixel data stored in CPU memory (RAM)
(define-cstruct _Image
  ([data (_pointer-to _void)] ; Image raw data
   [width _int] ; Image base width
   [height _int] ; Image base height
   [mipmaps _int] ; Mipmap levels, 1 by default
   [format _int] ; Data format (PixelFormat type)
   ))

;; Texture, tex data stored in GPU memory (VRAM)
(define-cstruct _Texture
  ([id _uint] ; OpenGL texture id
   [width _int] ; Texture base width
   [height _int] ; Texture base height
   [mipmaps _int] ; Mipmap levels, 1 by default
   [format _int] ; Data format (PixelFormat type)
   ))

(define _Texture2D _Texture)

(define _TextureCubemap _Texture)

;; RenderTexture, fbo for texture rendering
(define-cstruct _RenderTexture
  ([id _uint] ; OpenGL framebuffer object id
   [texture _Texture] ; Color buffer attachment texture
   [depth _Texture] ; Depth buffer attachment texture
   ))

(define _RenderTexture2D _RenderTexture)

;; NPatchInfo, n-patch layout info
(define-cstruct _NPatchInfo
  ([source _Rectangle] ; Texture source rectangle
   [left _int] ; Left border offset
   [top _int] ; Top border offset
   [right _int] ; Right border offset
   [bottom _int] ; Bottom border offset
   [layout _int] ; Layout of the n-patch: 3x3, 1x3 or 3x1
   ))

;; GlyphInfo, font characters glyphs info
(define-cstruct _GlyphInfo
  ([value _int] ; Character value (Unicode)
   [offsetX _int] ; Character offset X when drawing
   [offsetY _int] ; Character offset Y when drawing
   [advanceX _int] ; Character advance position X
   [image _Image] ; Character image data
   ))

;; Font, font texture and GlyphInfo array data
(define-cstruct _Font
  ([baseSize _int] ; Base size (default chars height)
   [glyphCount _int] ; Number of glyph characters
   [glyphPadding _int] ; Padding around the glyph characters
   [texture _Texture2D] ; Texture atlas containing the glyphs
   [recs (_pointer-to _Rectangle)] ; Rectangles in texture for the glyphs
   [glyphs (_pointer-to _GlyphInfo)] ; Glyphs info data
   ))

;; Camera, defines position/orientation in 3d space
(define-cstruct _Camera3D
  ([position _Vector3] ; Camera position
   [target _Vector3] ; Camera target it looks-at
   [up _Vector3] ; Camera up vector (rotation over its axis)
   [fovy _float] ; Camera field-of-view apperture in Y (degrees) in perspective, used as near plane width in orthographic
   [projection _int] ; Camera projection: CAMERA_PERSPECTIVE or CAMERA_ORTHOGRAPHIC
   ))

(define _Camera _Camera3D)

;; Camera2D, defines position/orientation in 2d space
(define-cstruct _Camera2D
  ([offset _Vector2] ; Camera offset (displacement from target)
   [target _Vector2] ; Camera target (rotation and zoom origin)
   [rotation _float] ; Camera rotation in degrees
   [zoom _float] ; Camera zoom (scaling), should be 1.0f by default
   ))

;; Mesh, vertex data and vao/vbo
(define-cstruct _Mesh
  ([vertexCount _int] ; Number of vertices stored in arrays
   [triangleCount _int] ; Number of triangles stored (indexed or not)
   [vertices (_pointer-to _float)] ; Vertex position (XYZ - 3 components per vertex) (shader-location = 0)
   [texcoords (_pointer-to _float)] ; Vertex texture coordinates (UV - 2 components per vertex) (shader-location = 1)
   [texcoords2 (_pointer-to _float)] ; Vertex second texture coordinates (useful for lightmaps) (shader-location = 5)
   [normals (_pointer-to _float)] ; Vertex normals (XYZ - 3 components per vertex) (shader-location = 2)
   [tangents (_pointer-to _float)] ; Vertex tangents (XYZW - 4 components per vertex) (shader-location = 4)
   [colors (_pointer-to _ubyte)] ; Vertex colors (RGBA - 4 components per vertex) (shader-location = 3)
   [indices (_pointer-to _ushort)] ; Vertex indices (in case vertex data comes indexed)
   [animVertices (_pointer-to _float)] ; Animated vertex positions (after bones transformations)
   [animNormals (_pointer-to _float)] ; Animated normals (after bones transformations)
   [boneIds (_pointer-to _ubyte)] ; Vertex bone ids, max 255 bone ids, up to 4 bones influence by vertex (skinning)
   [boneWeights (_pointer-to _float)] ; Vertex bone weight, up to 4 bones influence by vertex (skinning)
   [vaoId _uint] ; OpenGL Vertex Array Object id
   [vboId (_pointer-to _uint)] ; OpenGL Vertex Buffer Objects id (default vertex data)
   ))

;; Shader
(define-cstruct _Shader
  ([id _uint] ; Shader program id
   [locs (_pointer-to _int)] ; Shader locations array (RL_MAX_SHADER_LOCATIONS)
   ))

;; Transform, vectex transformation data
(define-cstruct _Transform
  ([translation _Vector3] ; Translation
   [rotation _Quaternion] ; Rotation
   [scale _Vector3] ; Scale
   ))

;; Ray, ray for raycasting
(define-cstruct _Ray
  ([position _Vector3] ; Ray position (origin)
   [direction _Vector3] ; Ray direction
   ))

;; RayCollision, ray hit information
(define-cstruct _RayCollision
  ([hit _bool] ; Did the ray hit something?
   [distance _float] ; Distance to nearest hit
   [point _Vector3] ; Point of nearest hit
   [normal _Vector3] ; Surface normal of hit
   ))

;; BoundingBox
(define-cstruct _BoundingBox
  ([min _Vector3] ; Minimum vertex box-corner
   [max _Vector3] ; Maximum vertex box-corner
   ))

;; Wave, audio wave data
(define-cstruct _Wave
  ([frameCount _uint] ; Total number of frames (considering channels)
   [sampleRate _uint] ; Frequency (samples per second)
   [sampleSize _uint] ; Bit depth (bits per sample): 8, 16, 32 (24 not supported)
   [channels _uint] ; Number of channels (1-mono, 2-stereo, ...)
   [data (_pointer-to _void)] ; Buffer data pointer
   ))

;; AudioStream, custom audio stream
(define-cstruct _AudioStream
  ([buffer (_pointer-to _rAudioBuffer)] ; Pointer to internal data used by the audio system
   [sampleRate _uint] ; Frequency (samples per second)
   [sampleSize _uint] ; Bit depth (bits per sample): 8, 16, 32 (24 not supported)
   [channels _uint] ; Number of channels (1-mono, 2-stereo, ...)
   ))

;; Sound
(define-cstruct _Sound
  ([stream _AudioStream] ; Audio stream
   [frameCount _uint] ; Total number of frames (considering channels)
   ))

;; Music, audio stream, anything longer than ~10 seconds should be streamed
(define-cstruct _Music
  ([stream _AudioStream] ; Audio stream
   [frameCount _uint] ; Total number of frames (considering channels)
   [looping _bool] ; Music looping enable
   [ctxType _int] ; Type of music context (audio filetype)
   [ctxData (_pointer-to _void)] ; Audio context data, depends on type
   ))
"))
