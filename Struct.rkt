#lang racket

; import racket libraries
(require 2htdp/image)
(require 2htdp/universe)

(provide GameState GameState-quit)

(struct GameState(quit) #:transparent)