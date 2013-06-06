#lang racket

#|

This is a dummy version implemented in standard C++ that just pretends
to export data.

|#

(require "base.rkt")
(require "konffaa/util.rkt")
(require "konffaa/variant.rkt")

(define-variant*/default
  project-variant%
  (super-new)
  ) ;; end class
