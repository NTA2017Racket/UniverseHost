#lang racket

; import racket libraries
(require 2htdp/image)
(require 2htdp/universe)

(require "Structs.rkt")

(provide add-player)

(define (add-player state player) (GameState (GameState-quit state) (GameState-time state)
 (append (GameState-players state) player) (GameState-planets state)))