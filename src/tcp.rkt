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

(define (loop id in out)
    (define (LoopTask ClientInput id in out)
        (dln (string-append id "==>" ClientInput))
        (cond
            [
                (number? (string->number (getStringWithoutLineBreak ClientInput))) 
                (dln "Number")
            ]
            [
                (number? (string->number ClientInput)) 
                (lambda ()
                    (dln "Number")
                )
            ]
        )
        (loop id in out)
    )
    (define ClientInput (read-line in))
    (if (equal? ClientInput eof)
        (lambda ()
            (dln (string-append (nameAndID id) " disconnected."))
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
    (dln (string-append (nameAndID id) " connected!"))
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
    (define listener (tcp-listen port 5 #t))
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
        (printf "~a = ~s\n" k v)
        (cond
            ((port-closed? v)
                (dict-remove! CLIENTLIST k)
                (dln (string-append "Removed client" k "."))
            )
            (
                (dln (string-append "Send message to " k "."))
                (display (string-append message "\n") v)
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