 #lang racket
(require "structs_as_string.rkt")
;;!!! Consider figuring out how to stop the final closing paren from being on it's own new line
;; (define src (open-input-file "../unsafe/functions.rkt" #:mode 'text))

(define out (open-output-file "structs.rkt" #:exists 'replace))
;; global variables to be configured as the structs are read in.
;; will produce these values below
;; [make-Vector2 (-> Float Float Vector2)]
;; [Vector2-x (-> Vector2 Float)]
;; [Vector2-y (-> Vector2 Float)]

;; global 
(define *new-struct "")
(define *make-struct "")

;; preamble
(define foo "#lang typed/racket(provide Texture2D
         Texture2D-id
         Texture2D-width
         Texture2D-height
         Texture2D-mipmaps
         Texture2D-format)

(require/typed/provide \"../unsafe/structs.rkt\"
  [#:opaque Vector2 Vector2?]
  [make-Vector2 (-> Float Float Vector2)]
  [Vector2-x (-> Vector2 Float)]
  [Vector2-y (-> Vector2 Float)]

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
  (Texture-format tex))")
;; #lang racket/base

;; (require ffi/unsafe raylib/support)

;; (provide (all-defined-out))

;; ;; Vector2, 2 components
;; (define-cstruct _Vector2
;;   ([x _float] ; Vector x component
;;    [y _float] ; Vector y component
;;    ))

;; ;; Vector3, 3 components
;; (define-cstruct _Vector3
;;   ([x _float] ; Vector x component
;;    [y _float] ; Vector y component
;;    [z _float] ; Vector z component
;;    ))

;; ;; Vector4, 4 components
;; (define-cstruct _Vector4
;;   ([x _float] ; Vector x component
;;    [y _float] ; Vector y component
;;    [z _float] ; Vector z component
;;    [w _float] ; Vector w component
;;    ))

;; (define _Quaternion _Vector4)

;; ;; Matrix, 4x4 components, column major, OpenGL style, right handed
;; (define-cstruct _Matrix
;;   ([m0 _float] [m4 _float] [m8 _float] [m12 _float] ; Matrix first row (4 components)
;;    [m1 _float] [m5 _float] [m9 _float] [m13 _float] ; Matrix second row (4 components)
;;    [m2 _float] [m6 _float] [m10 _float] [m14 _float] ; Matrix third row (4 components)
;;    [m3 _float] [m7 _float] [m11 _float] [m15 _float] ; Matrix fourth row (4 components)
;;    ))




(display "#lang typed/racket\n" out)
(display "(provide Quaternion\n" out)
(display "         Texture2D\n" out)
(display "         TextureCubemap\n" out)
(display "         RenderTexture2D\n" out)
(display "         Camera)\n\n" out)


(display "(require/typed/provide ffi/unsafe\n" out)
(display "  [#:opaque Pointer cpointer?])\n\n" out)
(display "(require/typed/provide \"../structs..rkt\"\n" out)

(define (c->racket-type type str)
  ;; I know I can look for _var and replace _ and capitalize, just want to see all the types so I can decide how the
  ;; types should be represented
  (match type
    ["_bool" "Boolean"]
    ["_void" "Void"]
    ["_string" "String"]
    ["_int" "Integer"]
    ["_uint" "Integer"]
    ["_Image" "Image"]
    ["_Vector2" "Vector2"]
    ["_float" "Float"]
    ["_Color" "Color"]
    ["_Shader" "Shader"]
    ["_Texture" "Texture"]    
    ["_Texture2D" "Texture2D"]
    ["_Camera" "Camera"]
    ["_Camera2D" "Camera2D"]
    ["_Camera3D" "Camera3D"]
    ["_Matrix" "Matrix"]
    ["_Vector3" "Vector3"]
    ["_RenderTexture" "RenderTexture"]    
    ["_RenderTexture2D" "RenderTexture2D"]
    ["_VrStereoConfig" "VrStereoConfig"]
    ["_VrDeviceInfo" "VrDeviceInfo"]
    ["_VrStereoConfig" "VrStereoConfig"]
    ["_Ray" "Ray"]
    ["_double" "Float"]
    ["_TraceLogCallback" "TraceLogCallback"]
    ["_LoadFileDataCallback" "LoadFileDataCallback"]
    ["_SaveFileDataCallback" "SaveFileDataCallback"]
    ["_LoadFileTextCallback" "LoadFileTextCallback"]
    ["_SaveFileTextCallback" "SaveFileTextCallback"]
    ["_long" "Long"]
    ["_Rectangle" "Rectangle"]
    ["_NPatchInfo" "NPatchInfo"]
    ["_Vector4" "Vector4"]
    ["_Font" "Font"]
    ["_GlyphInfo" "GlyphInfo"]
    ["_Model" "Model"]
    ["_Mesh" "Mesh"]
    ["_BoundingBox" "BoundingBox"]
    ["_Material" "Material"]
    ["_ModelAnimation" "ModelAnimation"]
    ["_RayCollision" "RayCollision"]
    ["_Wave" "Wave"]
    ["_TextureCubemap" "TextureCubemap"]
    ["_byte" "Byte"]
    ["_ubyte" "Byte"]
    ["_Sound" "Sound"]
    ["_Music" "Music"]
    ["_AudioStream" "AudioStream"]
    [else (begin
            (println (format "type is : ~a str is:~a" type str))
            type)]))

(let loop ([str (read-line src)])
  (when (not (eof-object? str))
    (cond
      ;; easier to handle this sepratetly 
      [(string-contains? str "(define-cstruct _Matrix") (begin

                                                      (define struct-name (c->racket-type "Matrix" str))
                                                      ;; [#:opaque Vector2 Vector2?]
                                                      ;; [make-Vector2 (-> Float Float Vector2)]
                                                      ;; [Vector2-x (-> Vector2 Float)]
                                                      ;; [Vector2-y (-> Vector2 Float)]
                                                      (display *make-struct out)
                                                      (display (format " ~a)]\n\n" *new-struct) out)
                                                      (display
                                                       (format "  [#:opaque ~a ~a?]\n" struct-name struct-name) out)
                                                      (set! *new-struct struct-name)
                                                      (for ([num (in-range 16)])
                                                        (display (format "  [~a-~a (-> ~a ~a)]\n" *new-struct (string-append "m" (number->string num))
                                                              *new-struct "Float") out))

                                                      (set!
                                                       *make-struct (format "  [make-~a (-> Float Float Float Float Float Float Float Float Float Float Float Float Float Float Float Float Matrix" struct-name)))]

      
      [(string-contains? str "(define-cstruct") (begin
                                                  (define struct-match (regexp-match
                                                                        ".define-cstruct (.*)\r$" str))
                                                  (when struct-match
                                                    (define struct-name (c->racket-type (cadr struct-match) str))
                                                    (when (string=? *new-struct "")
                                                      (set! *new-struct struct-name)
                                                      (display
                                                       (format "  [#:opaque ~a ~a?]\n" struct-name struct-name) out)
                                                      (set! *make-struct (format "  [make-~a (->" struct-name)))

                                                    (when (not (string=? *new-struct ""))
                                                      (define struct-name (c->racket-type (cadr struct-match) str))

                                                      ;; pop off the previous structs' constructor
                                                      ;; (println *make-struct)
                                                      (display *make-struct out)
                                                      (display (format " ~a)]\n\n" *new-struct) out)
                                                      (display
                                                       (format "  [#:opaque ~a ~a?]\n" struct-name struct-name) out)
                                                      (set! *new-struct struct-name)
                                                      (set!
                                                       *make-struct (format "  [make-~a (->" struct-name)))

                                                    (unless struct-match
                                                      (println str))))]

      [(string-contains? str "(_pointer-to")   (begin

                                                 (define match-var (regexp-match "\\[(.*) :.*" str))
                                                 (when match-var
                                                   (define type "Pointer")
                                                   (define var (cadr match-var))
                                                   ;; (when (string=? var "r")
                                                   ;;   (println type)
                                                   

                                                   ;;   (println (format "the type is ~a" (c->racket-type type str))))
                                                   (set! *make-struct (string-append *make-struct (format " ~a" (c->racket-type type str))))
                                                   (display (format "  [~a-~a (-> ~a ~a)]\n" *new-struct var
                                                                    *new-struct (c->racket-type type str)) out))
                                                 (unless match-var
                                                   (println str)))]
      

      [(string-contains? str "[") (begin
                                    (define new-match (regexp-match "\\(?\\[[A-z]+ (.*)\\]" str))
                                    (define match-var (regexp-match "\\(?\\[(.*) .*\\]" str))
                                    (when new-match
                                      (define type (c->racket-type (cadr new-match) str))
                                      (define var (cadr match-var))
                                      ;; (when (string=? var "r")
                                      ;;   (println type)
                                        

                                      ;;   (println (format "the type is ~a" (c->racket-type type str))))
                                      (set! *make-struct (string-append *make-struct (format " ~a" (c->racket-type type str))))
                                      (display (format "  [~a-~a (-> ~a ~a)]\n" *new-struct var
                                                       *new-struct (c->racket-type type str)) out))
                                      (unless new-match
                                        (println str)))]

                                     )
    (loop (read-line src))))

(display *make-struct out)
;; closing the last paren and finishing off the expression
(display (format " ~a)])\n" *new-struct) out)

(display (format "\n\n") out)

(display "(define-type Quaternion Vector4)\n" out)
(display "(define-type Texture2D Texture)\n" out)
(display "(define-type TextureCubemap Texture)\n" out)
(display "(define-type RenderTexture2D RenderTexture)\n" out)
(display "(define-type Camera Camera3D)\n" out)


(close-output-port out)
