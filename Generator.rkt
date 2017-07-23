#lang racket

; import racket libraries
(require 2htdp/image)
(require 2htdp/universe)

(require "Struct.rkt")

(provide generate-planets)

(define (render-planet rad col) (circle rad "solid" col))

(define (generate-planets num)
    (list 
    (Planet (Position 100 100) 20 (render-planet 20 "red"))
    (Planet (Position 200 300) 26 (render-planet 26 "red"))
    (Planet (Position 600 600) 30 (render-planet 30 "red"))
    )
)