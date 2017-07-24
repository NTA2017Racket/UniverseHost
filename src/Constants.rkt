#lang racket

; import racket libraries
(require 2htdp/image)
(require 2htdp/universe)

; This file defines universal constants.

(provide DEFAULTNAME WINDOW.HEIGHT WINDOW.WIDTH BACKGROUNDTEXTURE BACKGROUNDTEXTURE.HEIGHT BACKGROUNDTEXTURE.WIDTH PLANETTEXTURE SUNTEXTURE MOONTEXTURE)

(define WINDOW.HEIGHT 675)
(define WINDOW.WIDTH 1200)

(define BACKGROUNDTEXTURE (bitmap "../assets/Background.png"))
(define BACKGROUNDTEXTURE.WIDTH (image-width BACKGROUNDTEXTURE))
(define BACKGROUNDTEXTURE.HEIGHT (image-height BACKGROUNDTEXTURE))
(define PLANETTEXTURE (bitmap "../assets/Planet.png"))
(define SUNTEXTURE (bitmap "../assets/Sun.png"))
(define MOONTEXTURE (bitmap "../assets/Moon.png"))

(define DEFAULTNAME "anonym")