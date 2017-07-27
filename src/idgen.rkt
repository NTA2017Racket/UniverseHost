#lang racket

(provide get-new-id)

(define last-id 1)

(define (get-new-id)
    (set! last-id (+ last-id 1))
    last-id
)