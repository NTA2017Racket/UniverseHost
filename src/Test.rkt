#lang racket

(require "VectorMath.rkt")
(require "Struct.rkt")

(define vecs (list (Vector2D 20 10) (Vector2D 10 5) (Vector2D 1 2)))
(writeln (vector-min vecs))