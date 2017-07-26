#lang racket

; import racket libraries
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

; import other modules
(require "Struct.rkt")
(require "Constants.rkt")
(require "Generator.rkt")
(require "Functions.rkt")
(require "VectorMath.rkt")

; defines temporary variables. Should be moved.

(define BACKGROUND 
    (scale/xy 
        (/ WINDOW.WIDTH BACKGROUNDTEXTURE.WIDTH)
        (/ WINDOW.HEIGHT BACKGROUNDTEXTURE.HEIGHT)
    BACKGROUNDTEXTURE
    )
)

(define PLANETS 
    (list (Planet (Vector2D 400 400) 50 PLANETTEXTURE))
)

(define PLAYERS
    (list (Player 1 "Karl" (position-player PLANETS) 10 "red") (Player 2 "Dennis" (position-player PLANETS) 10 "green") (Player 3 "Ronald" (position-player PLANETS) 20 "white"))
)

(define PROJ (list (Projectile 1 (Vector2D 200 200) (Vector2D 0 0) (Vector2D 0 0) #true "red") (Projectile 2 (Vector2D 400 200) (Vector2D 0 0) (Vector2D 0 0) #true "green")))

; Render parts of screen
; Render parts of screen
(define (render-player-hud pl)
    (define str 
        (number->string
            (round
                (Player-energy pl)
            )
        )
    )
    (text
        (string-append
            (Player-name pl)
            ": "
           (substring 
                str
                0
                (- 
                    (string-length str)
                    2
                )
           )
        )
        20
        (Player-color pl)
    )
)

(define (render-counter num col)
    (define str 
        (number->string
            (round
                (/
                    (exact->inexact num)
                28)
            )
        )
    )
    (text 
        (substring
            str
            0
            (- 
                (string-length str)
                2
            )
        )
        30
        "white"
    )
)

(define (render-projectile proj)
    (circle 5 "solid" (Projectile-color proj))
)

; Render main function

(define (render state) 
    (place-images 
        (append
            (list 
                (render-counter
                    (GameState-time state)
                "white"
                )
            ) 
            (map
                (lambda
                    (pl)
                    (render-player-hud pl)
                )
                (GameState-players state)
            )
            (map 
                (lambda 
                    (p) 
                    (Planet-image p)
                ) 
                (GameState-planets state)
            )
            (map
                (lambda
                    (p)
                    (render-projectile p)
                )
                (GameState-projectiles state)
            )
        )
        (append 
            (list 
                (make-posn 50 50)
            )
            (map
                (lambda
                    (p)
                    (make-posn (+ (* (index-of (GameState-players state) p) 200) 100) 600)
                )
                (GameState-players state)
            ) 
            (map 
                (lambda
                    (p)
                    (convert-posn
                        (Planet-pos p)
                    )
                )
                (GameState-planets state)
            )
            (map
                (lambda
                    (p)
                    (convert-posn
                        (Projectile-pos p)
                    )
                )
                (GameState-projectiles state)
            )
        )
    BACKGROUND
    )
)

(define 
    (stop-game state)
    (exit)
)

(define (calculate-gravity pos planets)
    (define vm (vector-min
        (for/list ((i planets)) (vector-sub (Planet-pos i) pos))
    ))
    (vector-div vm 10000)
)

(define 
    (key-press state a-key)
    (if (key=? a-key "escape")
        (stop-game state)
        state
    )
)

(define (handle-physics projectiles planets)
    (map
        (lambda (p)
                ; Update acclerations from gravity
                ; update velocity from accleration
            (struct-copy Projectile p
                (accleration (calculate-gravity (Projectile-pos p) planets))
                (velocity (vector-add (Projectile-velocity p) (Projectile-accleration p)))
                (pos (vector-add (Projectile-pos p) (Projectile-velocity p)))
            )
        )
    projectiles
    )
)

(define 
    (update state)
    (if (= (GameState-gc state) 400)(collect-garbage 'major)
    (collect-garbage 'incremental))
    (struct-copy
        GameState
        state
        (time
            (+
                (GameState-time state) 
            1)
        )
        (players
            (add-energy (GameState-players state))
        )
        (projectiles
            (handle-physics 
                (GameState-projectiles state)
                (GameState-planets state)
            )
        )
        (gc (if (= (GameState-gc state) 400) 0 (+ (GameState-gc state) 1)))
    )
)

(big-bang (GameState #false 0 PLAYERS PLANETS PROJ 0)
    (to-draw render)
    (on-key key-press)
    (on-tick update)
    (name "Universe")
)