#lang racket

(require "Struct.rkt")

(provide vector-add vector-sub vector-sum vector-div vector-mul vector-length vector-min vector-max)

(define (vector-add a b)
    (Vector2D (+ (Vector2D-x a) (Vector2D-x b)) (+ (Vector2D-y a) (Vector2D-y b)))
)

(define (vector-sub a b)
    (Vector2D (- (Vector2D-x a) (Vector2D-x b)) (- (Vector2D-y a) (Vector2D-y b)))
)

(define (vector-sum a)
    (Vector2D (lsum (for/list ((i a)) (Vector2D-x i))) (lsum (for/list ((i a)) (Vector2D-y i))))
)

(define (vector-div a b)
    (Vector2D (/ (Vector2D-x a) b) (/ (Vector2D-y a) b))
)

(define (vector-mul a b)
    (Vector2D (* (Vector2D-x a) b) (* (Vector2D-y a) b))
)

(define (vector-length a)
    (sqrt (+ (expt (Vector2D-x a) 2) (expt (Vector2D-x a) 2)))
)

(define (vector-min a)
    (argmin vector-length a)
)

(define (vector-max a)
    (argmax vector-length a)
)

(define (lsum L)
  (apply + L)
)