#lang s-exp "konffaa/single-variant.rkt"

#|

This is a mock version implemented in standard C++ that just pretends
to export data. Builds Lua from source.

|#

(super-from "cxx-mock.var.rkt")

(override-attr lua-link-as 'source-code)
