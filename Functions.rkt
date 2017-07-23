#lang racket

; import racket libraries
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

(require "Struct.rkt")

(provide add-player convert-posn)

(define (add-player state player) (GameState (GameState-quit state) (GameState-time state)
 (append (GameState-players state) player) (GameState-planets state)))

(define (convert-posn pos) (make-posn (Position-x pos) (Position-y pos)))