#lang racket

(require misc1/syntax)
(require libuuid)

(define (handle in out)
    (display "HALLO\n" out)
    (define id (uuid-generate))
    (define (loop)
        (define input (read-line in))
        (display (string-append "UUID: " id " | " input "\n") (current-output-port))
        (loop)
    )
    (loop)
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

;(serve 8080)

(provide serve)