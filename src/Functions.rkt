#lang racket

; import racket libraries
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

(require "Struct.rkt")
(require "Constants.rkt")

(provide add-player convert-posn get-object-texture add-energy position-player)

(define (add-player state player)
    (struct-copy 
        GameState 
        state 
        (players 
            (append 
                (GameState-players state)
                player
            )
        )
    )
)

(define (convert-posn pos)
    (make-posn 
        (Position-x pos) 
        (Position-y pos)
    )
)

(define (get-object-texture rad)
    (cond
        ((> rad 50) SUNTEXTURE)
        ((< rad 27) MOONTEXTURE)
        (else PLANETTEXTURE)
    )
)

(define (add-energy players)
    (map 
        (lambda 
            (p) 
            (struct-copy 
                Player 
                p
                (energy
                    (+ 
                        (Player-energy p)
                    0.2)
                )
            )
        ) 
        players
    )
)

(define (position-player planets) 
    (define xpos (random 50 (- WINDOW.WIDTH 50)))
    (define ypos (random 50 (- WINDOW.HEIGHT 50)))
    (Position xpos ypos)
)

(define (planet-collision planet x y)
    (define poss (Planet-pos planet))
    (< (distance x y (Position-x poss) (Position-y poss)) (Planet-radius planet))
)

(define (distance x y x2 y2)
    (define distx (- x x2))
    (define disty (- y y2))
    (sqrt (+ (* distx distx) (* disty disty)))
)