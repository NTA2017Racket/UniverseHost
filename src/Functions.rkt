#lang racket

; import racket libraries
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

(require "Struct.rkt")
(require "Constants.rkt")

(provide add-player convert-posn get-object-texture add-energy)

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

(define (convert-posn pos) (make-posn (Position-x pos) (Position-y pos)))

(define (get-object-texture rad)
    (cond
        ((> rad 50) SUNTEXTURE)
        ((< rad 25) MOONTEXTURE)
        (else PLANETTEXTURE)
    )
)

(define (add-energy players)
    (map 
        (lambda 
            (p) 
            (+ 
                (Player-energy p)
            2)
        ) 
        players
    )
)