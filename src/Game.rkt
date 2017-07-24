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

; Render parts of screen
(define (render-player-hud pl)
    (define str (number->string
                (round
                    (Player-energy pl)
        )
    ))
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
        )
    BACKGROUND
    )
)

(define 
    (stop-game state)
    (exit)
)


(define (update-projectiles state)
    (define proj (GameState-projectiles state))
    (define newproj
        (map
            (lambda (p)
                ;Update acclerations from gravity
                ; update velocity from accleration
                (struct-copy Projectile p
                (velocity (+ (Projectile-velocity p) (Projectile-accleration))))
            )
            (proj)
        )
    )
    (struct-copy GameState state
        (projectiles newproj)
    )
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
    )
)

(big-bang (GameState #false 0 PLAYERS PLANETS (list))
    (to-draw render)
    (on-key key-press)
    (on-tick update)
    (name "Universe")
)