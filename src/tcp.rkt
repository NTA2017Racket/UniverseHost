#lang racket

;De:
;alexis-util
;libuuid
;parsack

(require libuuid)
(require "Struct.rkt")
(require readline)
(require readline/readline)
(install-readline!)
(require racket/future)
(require alexis/util/abbreviations)
(require "EventManager.rkt")
(require 2htdp/batch-io)
(require parsack)
(require "Functions.rkt")
(require mzlib/string)

(define (loop id in out)
    (define (LoopTask ClientInput id in out)
        (display (string-append (nameAndID id) ClientInput "\n> "))
        (cond
            [
                (number? (string->number (getStringWithoutLineBreak ClientInput))) 
                (_PlayerShoot id (string->number (getStringWithoutLineBreak ClientInput)))
            ]
            [
                (equal? (string-ref ClientInput 0) #\c) 
                (define commandSplit (string-split ClientInput #rx"\\s"))
                (if (> (length commandSplit) 1)
                    (_PlayerHasChangedName id (list-ref commandSplit 1))
                    (
                        (display "Bad syntax" out)
                        (flush-output out)
                    )
                )
            ]
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
    (define id (uuid-generate))
    (_addNewPlayer id)
    (display (string-append (nameAndID id) "connected!\n> "))
    (dict-set! CLIENTLIST id out)
    (display (string-append (read-file "Welcome") "\n") out)
    (flush-output out)
    (display "> " out)
    (flush-output out)
    (thread
        (loop id in out)
    )
)

(define (accept-and-handle listener)
    (define-values (in out) (tcp-accept listener))
    (handle in out)
    (close-input-port in)
    (close-output-port out)
)

(define (serve port)
    (thread (lambda ()
        (startREPL)
    ))
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

(define (brodcast message)
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

(define (startREPL)
    (define (replLoop)
        (define input (readline "> "))
        (brodcast input)
        (replLoop)
    )
    (replLoop)
)

; Display with new line
(define (dln message)
    (display (string-append message "\n"))
)

(serve 8080)

(provide serve brodcast)