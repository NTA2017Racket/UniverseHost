#lang racket

; import racket libraries
(require 2htdp/image)
(require 2htdp/universe)

(provide GameState GameState-quit GameState-time GameState-players GameState-planets Player Player-id Player-name Player-pos Player-energy Position Position-x Position-y
Planet Planet-pos Planet-radius Planet-image)

(struct GameState(quit time players planets) #:transparent)
(struct Player(id name pos energy) #:transparent)
(struct Position(x y) #:transparent)
(struct Planet(pos radius image) #:transparent)