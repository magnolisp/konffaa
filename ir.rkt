#lang racket/base

#|

Defines an internal representation for configuration information.

|#

(require "util.rkt")

(define-struct* VarCls
  (name ;; variant name
   bases ;; base classes
   ctor) ;; constructor function (not for bases)
  #:transparent)

(define-struct* VarObj
  (class ;; variant class
   name ;; file basename or class name
   attrs ;; attributes (functions)
   axioms ;; axioms (functions)
   cache) ;; attribute values
  #:transparent)

(define (run-ctors class obj)
  (define (run class)
    (for ((base (in-list (reverse (VarCls-bases class)))))
      (run base))
    (define ctor (VarCls-ctor class))
    (ctor obj))
  (run class))

(define (default-name class)
  (symbol->string (VarCls-name class)))

(define* (make-VarObj class #:name [name (default-name class)])
  (define obj (VarObj class name (make-hasheq) (make-hasheq) (make-hasheq)))
  (run-ctors class obj)
  (unless (hash-empty? (VarObj-cache obj))
    (error 'make-VarObj "unexpected early cache updates detected"))
  obj)

(define* (has-attr? obj name)
  (hash-has-key? (VarObj-attrs obj) name))

(define (default-nf obj name)
  (define class (VarObj-class obj))
  (define vname (VarCls-name class))
  (error 'get-attr! "no attribute ~a for variant ~a"
         name (VarObj-name obj)))

(define* (get-attr! obj name [not-found default-nf])
  (let ((cache (VarObj-cache obj)))
    (cond
      [(hash-has-key? cache name)
       (hash-ref cache name)]
      [else
       (define attrs (VarObj-attrs obj))
       (define compute (hash-ref attrs name not-found))
       (define v (compute))
       (hash-set! cache name v)
       v])))

(define (get-all! obj get post cache)
  (for ([(k v) (get obj)])
    (unless (hash-has-key? cache k)
      (let ((v (post v)))
        (hash-set! cache k v))))
  cache)

(define* (get-all-attrs! obj)
  (let ((cache (VarObj-cache obj)))
    (get-all! obj VarObj-attrs (lambda (v) (v)) cache)))

(define* (get-all-axioms obj)
  (let ((cache (make-hasheq)))
    (get-all! obj VarObj-axioms (lambda (v) v) cache)))

(define* (sort-attrs h)
  (let* ((lst (hash-map h (lambda (k v) (list k v)))))
    (sort lst symbol<? #:key car)))

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
