#lang racket

; import racket libraries
(require 2htdp/image)
(require 2htdp/universe)

(provide GameState GameState-quit GameState-time GameState-players GameState-planets GameState-gc
Player Player-id Player-name Player-pos Player-energy Player-color
Vector2D Vector2D-x Vector2D-y
Planet Planet-pos Planet-radius Planet-image
Projectile Projectile-uuid Projectile-pos Projectile-velocity Projectile-accleration Projectile-pool Projectile-color
TcpEvent TcpEvent-type TcpEvent-uuid TcpEvent-data)

(struct GameState(quit time players planets gc) #:transparent)
(struct Player(id name pos energy color) #:transparent)
(struct Vector2D(x y) #:transparent)
(struct Planet(pos radius image) #:transparent)
(struct Projectile(uuid pos velocity accleration pool color) #:transparent)
(struct TcpEvent(type uuid data) #:transparent)