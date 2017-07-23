#lang racket

; import racket libraries
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

; import other modules
(require "Struct.rkt")
(require "Constants.rkt")
(require "Generator.rkt")
(require "Functions.rkt")

(define BACKGROUND (scale/xy (/ WINDOW.WIDTH BACKGROUNDTEXTURE.WIDTH) (/ WINDOW.HEIGHT BACKGROUNDTEXTURE.HEIGHT) BACKGROUNDTEXTURE))
(define PLANETS (generate-planets 5))

(define (render-counter num col)
    (text 
    (number->string
    (round (/
        (exact->inexact num)
     28)))
    30 "white"))

(define (render state) 
    (place-images 
    (append (list (render-counter (GameState-time state) "white")) (map (lambda (p) (Planet-image p)) (GameState-planets state)))
    (append (list (make-posn 50 50)) (map (lambda (p) (convert-posn (Planet-pos p)))(GameState-planets state)))
    BACKGROUND)
)

(define (stop-game state) (exit))

(define (key-press state a-key) (if (key=? a-key "escape") (stop-game state) (state)))

(define (update state) (GameState #false (+ (GameState-time state) 1) (GameState-players state) (GameState-planets state)))

(big-bang (GameState #false 0 (list) PLANETS)
    (to-draw render)
    (on-key key-press)
    (on-tick update)
    (name "Universe")
)