#lang racket/base

#|

Syntax for specifying variant configurations.

There can be variation in feature sets, target platforms, or
toolchains, for example.

The variant specifications should strive to be fairly high-level, so
that only the actual variable choices can be made there. A
specification should pretty much be just a set of key/value pairs,
which we call attributes.

To avoid duplication in specifications, inheritance is supported, by
allowing multiple inheritance of base configurations, specified in
decreasing priority order.

|#

(require "ir.rkt" "util.rkt"
         racket/stxparam
         (for-syntax racket/base racket/syntax syntax/parse))

(define-syntax-parameter* self
  (syntax-rules ()))

(define-syntax* (define-attribute stx)
  (syntax-parse stx
    [(_ name:id v:expr ...+)
     #'(begin
         (define-syntax name
           (syntax-id-rules (name)
             [name (get-attr! self 'name)]))
         (hash-set! (VarObj-attrs self) 'name (lambda () v ...)))]))

(define-syntax* (define-axiom stx)
  (syntax-parse stx
    [(_ name:id v:expr ...+)
     #'(hash-set! (VarObj-axioms self) 'name (lambda () v ...))]))

(define-syntax* (variant-class stx)
  (syntax-parse stx
    [(_ name:id (base:expr ...) body ...+)
     (define/with-syntax obj (generate-temporary 'this))
     #'(VarCls 'name (list base ...)
               (lambda (obj)
                 (syntax-parameterize ([self (make-rename-transformer #'obj)])
                   body ...)))]))

(define-syntax-rule*
  (define-variant name (base ...) body ...)
  (define name
    (variant-class name (base ...) body ...)))

(define-syntax-rule*
  (define-variant* name (base ...) body ...)
  (begin
    (define-variant name (base ...) body ...)
    (provide name)))

(define-syntax* (define-variant** stx)
  (syntax-parse stx
    [(_ name (base ...) body ...)
     (define/with-syntax main-name (datum->syntax stx 'klass%))
     #'(begin
         (define-variant name (base ...) body ...)
         (provide name (rename-out [name main-name])))]))

(define-struct* hexnum (num))

(define* (class-all-attrs cls)
  (define obj
    (make-VarObj cls))
  (get-all-attrs! obj))

#|

Copyright 2009-2016 Helsinki Institute for Information
Technology (HIIT), University of Bergen, and the authors. All rights
reserved.

Authors: Tero Hasu <tero.hasu@hut.fi>

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation files
(the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge,
publish, distribute, sublicense, and/or sell copies of the Software,
and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

|#
