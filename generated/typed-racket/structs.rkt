#lang typed/racket
(provide Quaternion
         Texture2D
         TextureCubemap
         RenderTexture2D
         Camera)

(require/typed/provide ffi/unsafe
  [#:opaque Pointer cpointer?])

(require/typed/provide "../structs..rkt"
  [#:opaque Vector2 Vector2?]
  [make-Vector2 (-> Vector2)]

  [#:opaque Vector2 Vector2?]
  [Vector2-x (-> Vector2 Float)]
  [Vector2-y (-> Vector2 Float)]
  [make-Vector2 (-> Float Float Vector2)]

  [#:opaque Vector3 Vector3?]
  [Vector3-x (-> Vector3 Float)]
  [Vector3-y (-> Vector3 Float)]
  [Vector3-z (-> Vector3 Float)]
  [make-Vector3 (-> Float Float Float Vector3)]

  [#:opaque Vector4 Vector4?]
  [Vector4-x (-> Vector4 Float)]
  [Vector4-y (-> Vector4 Float)]
  [Vector4-z (-> Vector4 Float)]
  [Vector4-w (-> Vector4 Float)]
  [make-Vector4 (-> Float Float Float Float Vector4)]

  [#:opaque Matrix Matrix?]
  [Matrix-m0 (-> Matrix Float)]
  [Matrix-m1 (-> Matrix Float)]
  [Matrix-m2 (-> Matrix Float)]
  [Matrix-m3 (-> Matrix Float)]
  [Matrix-m4 (-> Matrix Float)]
  [Matrix-m5 (-> Matrix Float)]
  [Matrix-m6 (-> Matrix Float)]
  [Matrix-m7 (-> Matrix Float)]
  [Matrix-m8 (-> Matrix Float)]
  [Matrix-m9 (-> Matrix Float)]
  [Matrix-m10 (-> Matrix Float)]
  [Matrix-m11 (-> Matrix Float)]
  [Matrix-m12 (-> Matrix Float)]
  [Matrix-m13 (-> Matrix Float)]
  [Matrix-m14 (-> Matrix Float)]
  [Matrix-m15 (-> Matrix Float)]
  [make-Matrix (-> Float Float Float Float Float Float Float Float Float Float Float Float Float Float Float Float Matrix)]

  [#:opaque Color Color?]
  [Color-r (-> Color Byte)]
  [Color-g (-> Color Byte)]
  [Color-b (-> Color Byte)]
  [Color-a (-> Color Byte)]
  [make-Color (-> Byte Byte Byte Byte Color)]

  [#:opaque Rectangle Rectangle?]
  [Rectangle-x (-> Rectangle Float)]
  [Rectangle-y (-> Rectangle Float)]
  [Rectangle-width (-> Rectangle Float)]
  [Rectangle-height (-> Rectangle Float)]
  [make-Rectangle (-> Float Float Float Float Rectangle)]

  [#:opaque Image Image?]
  [Image-width (-> Image Integer)]
  [Image-height (-> Image Integer)]
  [Image-mipmaps (-> Image Integer)]
  [Image-format (-> Image Integer)]
  [make-Image (-> Integer Integer Integer Integer Image)]

  [#:opaque Texture Texture?]
  [Texture-id (-> Texture Integer)]
  [Texture-width (-> Texture Integer)]
  [Texture-height (-> Texture Integer)]
  [Texture-mipmaps (-> Texture Integer)]
  [Texture-format (-> Texture Integer)]
  [make-Texture (-> Integer Integer Integer Integer Integer Texture)]

  [#:opaque RenderTexture RenderTexture?]
  [RenderTexture-id (-> RenderTexture Integer)]
  [RenderTexture-texture (-> RenderTexture Texture)]
  [RenderTexture-depth (-> RenderTexture Texture)]
  [make-RenderTexture (-> Integer Texture Texture RenderTexture)]

  [#:opaque NPatchInfo NPatchInfo?]
  [NPatchInfo-source (-> NPatchInfo Rectangle)]
  [NPatchInfo-left (-> NPatchInfo Integer)]
  [NPatchInfo-top (-> NPatchInfo Integer)]
  [NPatchInfo-right (-> NPatchInfo Integer)]
  [NPatchInfo-bottom (-> NPatchInfo Integer)]
  [NPatchInfo-layout (-> NPatchInfo Integer)]
  [make-NPatchInfo (-> Rectangle Integer Integer Integer Integer Integer NPatchInfo)]

  [#:opaque GlyphInfo GlyphInfo?]
  [GlyphInfo-value (-> GlyphInfo Integer)]
  [GlyphInfo-offsetX (-> GlyphInfo Integer)]
  [GlyphInfo-offsetY (-> GlyphInfo Integer)]
  [GlyphInfo-advanceX (-> GlyphInfo Integer)]
  [GlyphInfo-image (-> GlyphInfo Image)]
  [make-GlyphInfo (-> Integer Integer Integer Integer Image GlyphInfo)]

  [#:opaque Font Font?]
  [Font-baseSize (-> Font Integer)]
  [Font-glyphCount (-> Font Integer)]
  [Font-glyphPadding (-> Font Integer)]
  [Font-texture (-> Font Texture2D)]
  [make-Font (-> Integer Integer Integer Texture2D Font)]

  [#:opaque Camera3D Camera3D?]
  [Camera3D-position (-> Camera3D Vector3)]
  [Camera3D-target (-> Camera3D Vector3)]
  [Camera3D-up (-> Camera3D Vector3)]
  [Camera3D-fovy (-> Camera3D Float)]
  [Camera3D-projection (-> Camera3D Integer)]
  [make-Camera3D (-> Vector3 Vector3 Vector3 Float Integer Camera3D)]

  [#:opaque Camera2D Camera2D?]
  [Camera2D-offset (-> Camera2D Vector2)]
  [Camera2D-target (-> Camera2D Vector2)]
  [Camera2D-rotation (-> Camera2D Float)]
  [Camera2D-zoom (-> Camera2D Float)]
  [make-Camera2D (-> Vector2 Vector2 Float Float Camera2D)]

  [#:opaque Mesh Mesh?]
  [Mesh-vertexCount (-> Mesh Integer)]
  [Mesh-triangleCount (-> Mesh Integer)]
  [Mesh-vaoId (-> Mesh Integer)]
  [make-Mesh (-> Integer Integer Integer Mesh)]

  [#:opaque Shader Shader?]
  [Shader-id (-> Shader Integer)]
  [make-Shader (-> Integer Shader)]

  [#:opaque _Transform _Transform?]
  [_Transform-translation (-> _Transform Vector3)]
  [_Transform-rotation (-> _Transform _Quaternion)]
  [_Transform-scale (-> _Transform Vector3)]
  [make-_Transform (-> Vector3 _Quaternion Vector3 _Transform)]

  [#:opaque Ray Ray?]
  [Ray-position (-> Ray Vector3)]
  [Ray-direction (-> Ray Vector3)]
  [make-Ray (-> Vector3 Vector3 Ray)]

  [#:opaque RayCollision RayCollision?]
  [RayCollision-hit (-> RayCollision Boolean)]
  [RayCollision-distance (-> RayCollision Float)]
  [RayCollision-point (-> RayCollision Vector3)]
  [RayCollision-normal (-> RayCollision Vector3)]
  [make-RayCollision (-> Boolean Float Vector3 Vector3 RayCollision)]

  [#:opaque BoundingBox BoundingBox?]
  [BoundingBox-min (-> BoundingBox Vector3)]
  [BoundingBox-max (-> BoundingBox Vector3)]
  [make-BoundingBox (-> Vector3 Vector3 BoundingBox)]

  [#:opaque Wave Wave?]
  [Wave-frameCount (-> Wave Integer)]
  [Wave-sampleRate (-> Wave Integer)]
  [Wave-sampleSize (-> Wave Integer)]
  [Wave-channels (-> Wave Integer)]
  [make-Wave (-> Integer Integer Integer Integer Wave)]

  [#:opaque AudioStream AudioStream?]
  [AudioStream-sampleRate (-> AudioStream Integer)]
  [AudioStream-sampleSize (-> AudioStream Integer)]
  [AudioStream-channels (-> AudioStream Integer)]
  [make-AudioStream (-> Integer Integer Integer AudioStream)]

  [#:opaque Sound Sound?]
  [Sound-stream (-> Sound AudioStream)]
  [Sound-frameCount (-> Sound Integer)]
  [make-Sound (-> AudioStream Integer Sound)]

  [#:opaque Music Music?]
  [Music-stream (-> Music AudioStream)]
  [Music-frameCount (-> Music Integer)]
  [Music-looping (-> Music Boolean)]
  [Music-ctxType (-> Music Integer)]
  [make-Music (-> AudioStream Integer Boolean Integer Music)])


(define-type Quaternion Vector4)
(define-type Texture2D Texture)
(define-type TextureCubemap Texture)
(define-type RenderTexture2D RenderTexture)
(define-type Camera Camera3D)
