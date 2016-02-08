#lang racket

#|

|#

(require konffaa/surface)

(define-variant** Symbian
  ((variant-class Arm ()
     (define-attribute x 4)
     (define-attribute z 8)))
  (define-attribute x (+ y ($ z)))
  (define-attribute y 1))
