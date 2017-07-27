#lang racket

; import racket libraries
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

(require "Struct.rkt")
(require "Constants.rkt")
(require "VectorMath.rkt")

(provide getStringWithoutLineBreak convert-posn get-object-texture add-energy position-player calc-velocity)

(define (calc-velocity angle)
    (define ra (* angle 0.00555555555 pi))
    (define xv (sin ra))
    (define yv (* (cos ra) -1))
    (vector-mul (Vector2D xv yv) 5)
)

(define (convert-posn pos)
    (make-posn 
        (Vector2D-x pos) 
        (Vector2D-y pos)
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

(define (calculate-grav x y planets)
1
)

(define (position-player planets) 
    (define xpos (random 50 (- WINDOW.WIDTH 50)))
    (define ypos (random 50 (- WINDOW.HEIGHT 50)))
    (Vector2D xpos ypos)
)

(define (planet-collision planet x y)
    (define poss (Planet-pos planet))
    (< (distance x y (Vector2D-x poss) (Vector2D-y poss)) (Planet-radius planet))
)

(define (distance x y x2 y2)
    (define distx (- x x2))
    (define disty (- y y2))
    (sqrt (+ (* distx distx) (* disty disty)))
)

(define (getStringWithoutLineBreak str)
    (define n (string-replace str "\n" ""))
    (define r (string-replace n "\r" ""))
    (string-replace r "\r\n" "")
)