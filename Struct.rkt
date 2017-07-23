#lang racket

; import racket libraries
(require 2htdp/image)
(require 2htdp/universe)

(provide GameState GameState-quit GameState-time Player Player-id Player-name Player-pos Player-energy Position Position-x Position-y)

(struct GameState(quit time) #:transparent)
(struct Player(id name pos energy) #:transparent)
(struct Position(x y) #:transparent)