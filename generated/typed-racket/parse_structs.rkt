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

;;!! make-image misses [Image-data (-> Image Pointer)] b/c it's a Pointer and it doesn't picked up
;; global 
(define *new-struct "")
(define *make-struct "")

;; preamble
(display "#lang typed/racket\n" out)
(display "(provide Quaternion\n" out)
(display "         Texture2D\n" out)
(display "         TextureCubemap\n" out)
(display "         RenderTexture2D\n" out)
(display "         Camera)\n\n" out)


(display "(require/typed/provide ffi/unsafe\n" out)
(display "  [#:opaque Pointer cpointer?])\n\n" out)
(display "(require/typed/provide \"../structs.rkt\"\n" out)

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
    ["_long" "Integer"]
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
    ["_Transform" "Transform"]
    ["_Quaternion" "Quaternion"]        
    [else (begin
            (println (format "type is : ~a str is:~a" type str))
            type)]))

(let loop ([str (read-line src)])
  (when (not (eof-object? str))
    (cond
      ;; easier to handle this sepratetly 
      [(string-contains? str "(define-cstruct _Matrix") (begin

                                                      (define struct-name (c->racket-type "Matrix" str))
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
