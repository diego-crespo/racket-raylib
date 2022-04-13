#lang racket
(require "functions_as_string.rkt")
;;!!! Consider figuring out how to stop the final closing paren from being on it's own new line
;; (define src (open-input-file "../unsafe/functions.rkt" #:mode 'text))

(define out (open-output-file "functions.rkt" #:exists 'replace))
;; preamble
(display "#lang typed/racket\n" out)
(display "(require \"structs.rkt\")\n" out)
(display "(require/typed/provide ffi/unsafe\n" out)
(display "  [#:opaque Pointer cpointer?])\n\n" out)
(display "(require/typed/provide \"../unsafe/functions.rkt\"\n" out)

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
      [(string-contains? str "(define-raylib") (begin
                                                 (define new-match (regexp-match
                                                                    ".define-raylib (.*)\r" str))
                                                 (when new-match
                                                   (define func-name (cadr new-match))
                                                   (display "  [" out)
                                                   (display func-name out)
                                                   (display " (-> " out))
                                                 (unless new-match
                                                   (println str)))]
      
      [(string-contains? str "(_pointer-to")   (begin
                                                 (define new-match (regexp-match
                                                                    "[-][>] \\(_pointer-to (.*)\\)\\)\\)\r" str))
                                                 (define new-match2 (regexp-match "\\[[A-z]+[0-9]? : \\(_pointer-to (.*)\\)\\]\r" str))                                                  
                                                 (when new-match
                                                   (define return-type (cadr new-match))
                                                   (display "Pointer" out)
                                                   (display ")]" out)
                                                   (display "\n" out))

                                                 (when new-match2
                                                   (define type (cadr new-match2))
                                                   (display "Pointer" out)
                                                   (display " " out))

                                                 (when (and (false? new-match) (false? new-match2))
                                                   (println str)))]

      [(string-contains? str " : ") (begin
                                      (define new-match (regexp-match "[[][A-z]+[0-9]? [:] (.*)]" str))
                                      (when new-match
                                        (define type (cadr new-match))
                                        (display (c->racket-type type str) out)
                                        (display " " out))
                                      (unless new-match
                                        (println str)))]


      [(string-contains? str "->") (begin
                                     (define new-match (regexp-match "[-][>] (.*)[)][)]\r" str))
                                     (when new-match
                                       (define return-type (cadr new-match))
                                       (display (c->racket-type return-type str) out)
                                       (display ")]" out)
                                       (display "\n" out))
                                     (unless new-match
                                       (println str)))])
    (loop (read-line src))))

;; closing the last paren
(display ")" out)
(close-output-port out)
