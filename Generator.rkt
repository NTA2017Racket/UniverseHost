#lang racket

; import racket libraries
(require 2htdp/image)
(require 2htdp/universe)

(require "Struct.rkt")
(require "Constants.rkt")

(provide generate-planets)

(define (render-planet rad col) (circle rad "solid" col))

(define (generate-planet)
    (define px (random 10 WINDOW.WIDTH))
    (define py (random 10 WINDOW.HEIGHT))
    (define rad (random 15 50))
    (Planet (Position px py) rad (render-planet rad "red"))
)

(define (generate-planets num)
    (map (lambda (n) (generate-planet)) (range num))
)