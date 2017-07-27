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
(require "tcp.rkt")
(require "TCPEvents.rkt")
(require "EventManager.rkt")

; defines temporary variables. Should be moved.

(define BACKGROUND 
    (scale/xy 
        (/ WINDOW.WIDTH BACKGROUNDTEXTURE.WIDTH)
        (/ WINDOW.HEIGHT BACKGROUNDTEXTURE.HEIGHT)
    BACKGROUNDTEXTURE
    )
)

(define PLANETS 
    (generate-planets 10)
)

(define PLAYERS
    (list (Player 1 "Karl" (position-player PLANETS) 10 "red") (Player 2 "Dennis" (position-player PLANETS) 10 "green") (Player 3 "Ronald" (position-player PLANETS) 20 "white"))
)

(define PROJ (list (Projectile 1 (Vector2D 200 200) (Vector2D 0 0) (Vector2D 0 0) #true "red") (Projectile 2 (Vector2D 400 200) (Vector2D 0 0) (Vector2D 0 0) #true "green")
(Projectile 3 (Vector2D 400 420) (Vector2D 00 0) (Vector2D 0 0) #true "blue") (Projectile 2 (Vector2D 100 420) (Vector2D 0 0) (Vector2D 0 0) #true "yellow")
(Projectile 1 (Vector2D 30 220) (Vector2D 0 0) (Vector2D 0 0) #true "white")
(Projectile 1 (Vector2D 400 440) (Vector2D 0 0) (Vector2D 0 0) #true "white")
(Projectile 1 (Vector2D 300 40) (Vector2D 0 0) (Vector2D 0 0) #true "white")
(Projectile 1 (Vector2D 10 20) (Vector2D 0 0) (Vector2D 0 0) #true "white")
(Projectile 1 (Vector2D 700 400) (Vector2D 0 0) (Vector2D 0 0) #true "white")
(Projectile 1 (Vector2D 350 140) (Vector2D 0 0) (Vector2D 0 0) #true "white")
(Projectile 1 (Vector2D 600 40) (Vector2D 0 0) (Vector2D 0 0) #true "white")
(Projectile 1 (Vector2D 300 90) (Vector2D 0 0) (Vector2D 0 0) #true "white")
))


; global vars

(define projectiles PROJ)
(define players PLAYERS)
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

(define (add-projectile pr)
    (set! projectiles (append projectiles (list pr)))
)

(define (remove-projectile pr)
    (set! projectiles (remove (list pr) projectiles))
)

(define (add-energy-frame)
    (set! players (add-energy))
)

(define (add-player pl)
    (set! players (append players (list pl)))
)

(define (remove-player pl)
    (set! players (remove (list pl) players))
)

(define (player-from-id id)
    ""
)

(define (handle-events state)
    (define events (getLatestEvent))
    (for-each (lambda (ev) (
        (cond
            ((equal? (TcpEvent-type ev) PLAYERJOINED)
            (add-player (Player (TcpEvent-uuid ev) DEFAULTNAME (position-player) 0 "red")))
            ((equal? (TcpEvent-type ev) PLAYERLEFT)
            (remove-player (player-from-id (TcpEvent-uuid ev)))
            )
            ((equal? (TcpEvent-type ev) PLAYERHASCHANGEDNAME)
            (define affected (player-from-id (TcpEvent-uuid ev)))
            (list-set players (index-of affected)
            (struct-copy Player affected (name (TcpEvent-data ev)))
            ))
            )
        )
    ))
    events)


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
                players
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
                projectiles
            )
        )
        (append 
            (list 
                (make-posn 50 50)
            )
            (map
                (lambda
                    (p)
                    (make-posn (+ (* (index-of players p) 200) 100) 600)
                )
                players
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
                projectiles
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
    (define f #false)
    (define vm (vector-min
        (for/list ((i planets)) (vector-sub (Planet-pos i) pos))
    ))
    (ProjectileUpdate (vector-mul vm 0.0001) (< (vector-length vm) 5))
)

(define 
    (key-press state a-key)
    (if (key=? a-key "escape")
        (stop-game state)
        state
    )
)

(define (update-physics planets)
    (for-each (lambda (p)
        (define pu (calculate-gravity (Projectile-pos p) planets))
        (define new (struct-copy Projectile p
            (accleration (ProjectileUpdate-vec pu))
            (velocity (vector-add (Projectile-velocity p) (Projectile-accleration p)))
            (pos (vector-add (Projectile-pos p) (Projectile-velocity p))))
        )
        (if (ProjectileUpdate-flag pu) (
            set! projectiles (remove p projectiles)
        ) (
            set! projectiles (list-set projectiles (index-of projectiles p) new)
        ))
    )
projectiles)
)


(define 
    (update state)
    ;(handle-events state)
    (update-physics (GameState-planets state))
    (struct-copy
        GameState
        state
        (time
            (+
                (GameState-time state) 
            1)
        )
    )
)

(thread (lambda () (start-server)))

(big-bang (GameState #false 0 PLANETS)
    (to-draw render)
    (on-key key-press)
    (on-tick update)
    (name "Universe")
)