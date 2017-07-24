#lang racket

; import racket libraries
(require 2htdp/image)
(require 2htdp/universe)

; This file defines universal constants.

(provide WINDOW.HEIGHT WINDOW.WIDTH BACKGROUNDTEXTURE BACKGROUNDTEXTURE.HEIGHT BACKGROUNDTEXTURE.WIDTH PLANETTEXTURE)

(define WINDOW.HEIGHT 675)
(define WINDOW.WIDTH 1200)

(define BACKGROUNDTEXTURE (bitmap "../assets/Background.png"))
(define BACKGROUNDTEXTURE.WIDTH (image-width BACKGROUNDTEXTURE))
(define BACKGROUNDTEXTURE.HEIGHT (image-height BACKGROUNDTEXTURE))
(define PLANETTEXTURE (bitmap "../assets/Planet.png"))