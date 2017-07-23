#lang racket

; import racket libraries
(require 2htdp/image)
(require 2htdp/universe)

; import other modules
(require "Struct.rkt")
(require "Constants.rkt")

(define BACKGROUND (scale/xy (/ WINDOW.WIDTH BACKGROUNDTEXTURE.WIDTH) (/ WINDOW.HEIGHT BACKGROUNDTEXTURE.HEIGHT) BACKGROUNDTEXTURE))

(define (render state) BACKGROUND)

(define (stop-game state) (exit))

(define (key-press state a-key) (if (key=? a-key "escape") (stop-game state) (state)))

(define (update state) state)

(big-bang (GameState #false)
    (to-draw render)
    (on-key key-press)
    (name "Test")
)