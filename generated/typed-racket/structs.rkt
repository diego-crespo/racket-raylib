#lang typed/racket

(provide Texture2D
         Texture2D-id
         Texture2D-width
         Texture2D-height
         Texture2D-mipmaps
         Texture2D-format)

(require/typed/provide "../unsafe/structs.rkt"
  [#:opaque Vector2 Vector2?]
  [make-Vector2 (-> Float Float Vector2)]
  [Vector2-x (-> Vector2 Float)]
  [Vector2-y (-> Vector2 Float)]

  [#:opaque Vector3 Vector3?]
  [make-Vector3 (-> Float Float Float Vector3)]
  [Vector3-x (-> Vector3 Float)]
  [Vector3-y (-> Vector3 Float)]
  [Vector3-z (-> Vector3 Float)]

  [#:opaque Camera2D Camera2D?] 
  [make-Camera2D (-> Vector2 Vector2 Float Float Camera2D)]
  [Camera2D-offset (-> Camera2D Vector2)]
  [Camera2D-target (-> Camera2D Vector2)]
  [Camera2D-rotation (-> Camera2D Float)]
  [Camera2D-zoom (-> Camera2D Float)]  

  [#:opaque Rectangle Rectangle?]
  [make-Rectangle (-> Float Float Float Float Rectangle)]
  [Rectangle-x (-> Rectangle Float)]
  [Rectangle-y (-> Rectangle Float)]
  [Rectangle-width (-> Rectangle Float)]
  [Rectangle-height (-> Rectangle Float)]

  [#:opaque Color Color?]
  [make-Color (-> Byte Byte Byte Byte Color)]
  [Color-r (-> Color Byte)]
  [Color-g (-> Color Byte)]
  [Color-b (-> Color Byte)]
  [Color-a (-> Color Byte)]

  [#:opaque Texture Texture?]
  [make-Texture (-> Integer Integer Integer Integer Integer Texture)]
  [Texture-id (-> Texture Integer)]
  [Texture-width (-> Texture Integer)]
  [Texture-height (-> Texture Integer)]
  [Texture-mipmaps (-> Texture Integer)]
  [Texture-format (-> Texture Integer)]
  )

(define-type Texture2D Texture)

(: Texture2D-id (-> Texture2D  Integer))
(define (Texture2D-id tex)
  (Texture-id tex))

(: Texture2D-width (-> Texture2D  Integer))
(define (Texture2D-width tex)
  (Texture-width tex))

(: Texture2D-height (-> Texture2D  Integer))
(define (Texture2D-height tex)
  (Texture-height tex))

(: Texture2D-mipmaps (-> Texture2D  Integer))
(define (Texture2D-mipmaps tex)
  (Texture-mipmaps tex))

(: Texture2D-format (-> Texture2D  Integer))
(define (Texture2D-format tex)
  (Texture-format tex))
