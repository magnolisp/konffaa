#lang racket

#|

A `#lang` that is full-blown Racket, augmented with some configuration
specification DSL.

|#

(module reader syntax/module-reader konffaa/main)

(require "attribute.rkt" "axiom.rkt" "surface.rkt")

(provide
 (all-from-out racket
               "attribute.rkt" "axiom.rkt" "surface.rkt"))

