#lang typed/racket/base
(require "structs.rkt")
(provide (all-defined-out))

;; Predefinined colors from raylib.h

(define LIGHTGRAY  (make-Color 200 200 200 255 ))   ; Light Gray
(define GRAY       (make-Color 130 130 130 255 ))   ; Gray
(define DARKGRAY   (make-Color 80 80 80 255 ))      ; Dark Gray
(define YELLOW     (make-Color 253 249 0 255 ))     ; Yellow
(define GOLD       (make-Color 255 203 0 255 ))     ; Gold
(define ORANGE     (make-Color 255 161 0 255 ))     ; Orange
(define PINK       (make-Color 255 109 194 255 ))   ; Pink
(define RED        (make-Color 230 41 55 255 ))     ; Red
(define MAROON     (make-Color 190 33 55 255 ))     ; Maroon
(define GREEN      (make-Color 0 228 48 255 ))      ; Green
(define LIME       (make-Color 0 158 47 255 ))      ; Lime
(define DARKGREEN  (make-Color 0 117 44 255 ))      ; Dark Green
(define SKYBLUE    (make-Color 102 191 255 255 ))   ; Sky Blue
(define BLUE       (make-Color 0 121 241 255 ))     ; Blue
(define DARKBLUE   (make-Color 0 82 172 255 ))      ; Dark Blue
(define PURPLE     (make-Color 200 122 255 255 ))   ; Purple
(define VIOLET     (make-Color 135 60 190 255 ))    ; Violet
(define DARKPURPLE (make-Color 112 31 126 255 ))    ; Dark Purple
(define BEIGE      (make-Color 211 176 131 255 ))   ; Beige
(define BROWN      (make-Color 127 106 79 255 ))    ; Brown
(define DARKBROWN  (make-Color 76 63 47 255 ))      ; Dark Brown
(define WHITE      (make-Color 255 255 255 255 ))   ; White
(define BLACK      (make-Color 0 0 0 255 ))         ; Black
(define BLANK      (make-Color 0 0 0 0 ))           ; Blank (Transparent))
(define MAGENTA    (make-Color 255 0 255 255 ))     ; Magenta
(define RAYWHITE   (make-Color 245 245 245 255 ))   ; My own White (raylib logo))
