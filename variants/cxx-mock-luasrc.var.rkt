#lang s-exp "konffaa/single-variant.rkt"

#|

This is a mock version implemented in standard C++ that just pretends
to export data. Builds Lua from source.

|#

(super-from "cxx-mock.var.rkt")

(define/override (lua-link-as.attr)
  'source-code)
