#lang racket

; import racket libraries
(require 2htdp/image)
(require 2htdp/universe)

(require "Struct.rkt")
(require "Constants.rkt")
(require "Functions.rkt")

(provide generate-planets render-planet)

(define (render-planet rad) (scale/xy (/ rad 100) (/ rad 100) (get-object-texture rad)))

(define (generate-planet)
    (define px (random 10 WINDOW.WIDTH))
    (define py (random 10 WINDOW.HEIGHT))
    (define rad (random 15 65))
    (Planet (Vector2D px py) rad (render-planet rad))
)

(define (generate-planets num)
    (map (lambda (n) (generate-planet)) (range num))
)