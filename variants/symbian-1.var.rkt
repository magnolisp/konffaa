#lang racket

#|

|#

(require konffaa/surface)

(define-variant** Symbian
  ((variant-class Arm ()
     (define-attribute x 4)
     (define-field w 3)
     (define-attribute z 8)))
  (define-attribute x (+ y ($ z) ($ w)))
  (define-attribute y 1)
  (define-attribute yy (* 2 www))
  (define-field ww (* 2 x))
  (define-field www (* 2 ww)))

