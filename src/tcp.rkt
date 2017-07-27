#lang racket

;De:
;alexis-util
;libuuid
;parsack

(require "Struct.rkt")
;(require readline)
;(require readline/readline)
;(install-readline!)
(require racket/future)
(require alexis/util/abbreviations)
(require "EventManager.rkt")
(require 2htdp/batch-io)
(require parsack)
(require "Functions.rkt")
(require "idgen.rkt")
(require mzlib/string)

(define (start-server) (serve 8080))

(define (loop id in out)
    (define (LoopTask ClientInput id in out)
        (define (IHATETHISSHIT)
            (define commandSplit (string-split ClientInput " "))
            (define (SendThisPlayerAmessageBecauseWeChangedHisDisplaynameAndApplyIt)
                (display (string-append "[Server]: We changed your name to " (list-ref commandSplit 1) "\n") out)
                (display (string-append "A player changed his name to " (list-ref commandSplit 1) "\n> "))
                (_PlayerHasChangedName id (list-ref commandSplit 1))
            )
            (define (SendThePlayerASyntaxError)
                (display "Bad syntax\n" out)
                (flush-output out)
            )
            (if (equal? (length commandSplit) 2)
                (SendThisPlayerAmessageBecauseWeChangedHisDisplaynameAndApplyIt)
                (SendThePlayerASyntaxError)
            )
        )
        (define (ListPlayers)
            (define index 1)
            (dict-for-each (getAllPlayer) (lambda (k v)
                (display (string-append (number->string index) ". " v "\n") out)
                (set! index (+ index 1))
            ))
        )
        (cond
            [
                (number? (string->number (getStringWithoutLineBreak ClientInput))) 
                (display (string-append (nameAndID id) "fired with " ClientInput "\n"))
                (_PlayerShoot id (string->number (getStringWithoutLineBreak ClientInput)))
                (display (string-append (nameAndID id) "Successfully fired event!"  "\n> "))
            ]
            [
                (> (string-length ClientInput) 0)
                (cond
                    [
                        (equal? (string-ref ClientInput 0) #\c)
                        (IHATETHISSHIT)
                        (display (string-append (nameAndID id) ClientInput "\n> "))
                    ]
                    [
                        (equal? (string-ref ClientInput 0) #\l)
                        (ListPlayers)
                    ]
                )
            ]
            (else
                (display (string-append (nameAndID id) ClientInput "\n> "))
            )
        )
        (display "> " out)
        (flush-output out)
        (loop id in out)
    )
    (define ClientInput (read-line in))
    (if (equal? ClientInput eof)
        (lambda ()
            (dln (string-append (nameAndID id) "disconnected."))
            (dict-remove! CLIENTLIST id)
            (_removePlayer id)
            (kill-thread (current-thread))
        )
        (LoopTask ClientInput id in out)
    )
)

(define (handle in out)
    (define id (get-new-id))
    (_addNewPlayer id)
    (display (string-append (nameAndID id) "connected!\n> "))
    (dict-set! CLIENTLIST id out)
    (display (string-append (read-file "src/Welcome.txt") "\n") out)
    (flush-output out)
    (display "> " out)
    (flush-output out)
    (loop id in out)
)

(define (accept-and-handle listener)
    (define-values (in out) (tcp-accept listener))
    (thread
        (lambda ()
            (handle in out)
            (close-input-port in)
            (close-output-port out)
        )
    )
)

(define (serve port)
    ;#;(thread (lambda ()
    ;    (startREPL)
    ;))
    (define listener (tcp-listen port 8 #t))
    (define (loop)
        (accept-and-handle listener)
        (loop)
    )
    (define t (thread (loop)))
    (lambda ()
        (kill-thread t)
        (tcp-close listener)
    )
)

(define (broadcast message)
    (dict-for-each CLIENTLIST (lambda (k v)
        (cond
            ((port-closed? v)
                (dict-remove! CLIENTLIST k)
                (_removePlayer k)
            )
            (
                (display (string-append "[Server]: " message "\n> ") v)
                (flush-output v)
            )
        )
    ))
)

;(define (startREPL)
;    (define (replLoop)
;        (define input (readline "> "))
;        (brodcast input)
;        (replLoop)
;    )
;    (replLoop)
;)

; Display with new line
(define (dln message)
    (display (string-append message "\n"))
)

(define (start)
    (thread )
)

(provide serve broadcast start-server)

