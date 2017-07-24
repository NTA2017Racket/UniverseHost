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

; defines temporary variables. Should be moved.

(define BACKGROUND 
    (scale/xy 
        (/ WINDOW.WIDTH BACKGROUNDTEXTURE.WIDTH)
        (/ WINDOW.HEIGHT BACKGROUNDTEXTURE.HEIGHT)
    BACKGROUNDTEXTURE
    )
)

(define PLANETS 
    (generate-planets 20)
)

(define PLAYERS
    (list (Player 1 "Test" (position-player PLANETS) 10 "red") (Player 2 "Spieler" (position-player PLANETS) 10 "green") (Player 3 "Spieler" (position-player PLANETS) 20 "white"))
)

(define DOT (circle 5 "solid" "red"))

(define PROJ (list (Projectile 1 (Vector2D 200 200) (Vector2D 5 5) (Vector2D 1 0)) (Projectile 2 (Vector2D 400 200) (Vector2D -5 5) (Vector2D -1 0))))

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
    DOT
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
    (Vector2D 1 0)
)

(define 
    (key-press state a-key)
    (if (key=? a-key "escape")
        (stop-game state)
        state
    )
)

(define 
    (update state)
        (define newproj

        (map
            (lambda (p)
                ; Update acclerations from gravity
                ; update velocity from accleration
                (struct-copy Projectile p
                    (velocity (vector-add (Projectile-velocity p) (Projectile-accleration p)))
                    (pos (vector-add (Projectile-pos p) (Projectile-velocity p)))
                )
            )
            (GameState-projectiles state)
        )
        )
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
        (projectiles newproj)
    )
)

(big-bang (GameState #false 0 PLAYERS PLANETS PROJ)
    (to-draw render)
    (on-key key-press)
    (on-tick update)
    (name "Universe")
)