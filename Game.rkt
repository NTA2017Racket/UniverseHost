#lang racket

; import racket libraries
(require 2htdp/image)
(require 2htdp/universe)

; import other modules
(require "Struct.rkt")
(require "Constants.rkt")

(define BACKGROUND (scale/xy (/ WINDOW.WIDTH BACKGROUNDTEXTURE.WIDTH) (/ WINDOW.HEIGHT BACKGROUNDTEXTURE.HEIGHT) BACKGROUNDTEXTURE))

(define (render state) 
    (place-image 
    (text 
    (number->string
    (round (/
        (exact->inexact (GameState-time state))
     28)))
    30 "white")
    50 50 BACKGROUND)
)

(define (stop-game state) (exit))

(define (key-press state a-key) (if (key=? a-key "escape") (stop-game state) (state)))

(define (update state) (GameState #false (+ (GameState-time state) 1)))

(big-bang (GameState #false 0)
    (to-draw render)
    (on-key key-press)
    (on-tick update)
    (name "Universe")
)