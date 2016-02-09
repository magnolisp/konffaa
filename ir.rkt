#lang racket/base

#|

Defines an internal representation for configuration information.

|#

(require "util.rkt" racket/set)

(define-struct* VarCls
  (name ;; variant name
   bases ;; base classes
   ctor) ;; constructor function (not for bases)
  #:transparent)

(define-struct* VarObj
  (cls ;; variant class
   name ;; file basename or class name
   fields ;; non-attribute fields (functions) 
   attrs ;; attributes (functions)
   axioms ;; axioms (functions)
   cache) ;; attribute values
  #:transparent)

(define (run-ctors cls obj)
  (define (run cls)
    (for ((base (in-list (reverse (VarCls-bases cls)))))
      (run base))
    (define ctor (VarCls-ctor cls))
    (ctor obj))
  (run cls))

(define (default-name cls)
  (symbol->string (VarCls-name cls)))

(define* (make-VarObj cls #:name [name (default-name cls)])
  (define obj (VarObj cls name (make-hasheq) (make-hasheq)
                      (make-hasheq) (make-hasheq)))
  (run-ctors cls obj)
  (unless (hash-empty? (VarObj-cache obj))
    (error 'make-VarObj "unexpected early cache updates detected"))
  (let ((aset (list->mutable-seteq (hash-keys (VarObj-attrs obj))))
        (fset (list->mutable-seteq (hash-keys (VarObj-fields obj)))))
    (set-intersect! aset fset)
    (unless (set-empty? aset)
      (error 'make-VarObj
             "clashing names for attributes and fields: ~s"
             (set->list aset))))
  obj)

(define (not-found kind obj name)
  (define cls (VarObj-cls obj))
  (define vname (VarCls-name cls))
  (error 'not-found "no ~a ~a for variant ~a"
         kind name (VarObj-name obj)))

(define (get! obj name get nf)
  (let ((cache (VarObj-cache obj)))
    (hash-ref! cache name
               (lambda ()
                 (define h (get obj))
                 (define v (hash-ref h name nf))
                 (v)))))
  
(define* (get-field! obj name
                     [nf (lambda () (not-found "field" obj name))])
  (get! obj name VarObj-fields nf))

(define* (get-attr! obj name
                    [nf (lambda () (not-found "attribute" obj name))])
  (get! obj name VarObj-attrs nf))

(define (has-field? obj name)
  (hash-has-key? (VarObj-fields obj) name))

(define (has-attr? obj name)
  (hash-has-key? (VarObj-attrs obj) name))

(define* (get-attr-or-field! obj name)
  (cond
    [(has-attr? obj name)
     (get-attr! obj name)]
    [(has-field? obj name)
     (get-field! obj name)]
    [else
     (not-found "attribute or field" obj name)]))

(define (get-all! obj get)
  (let ((cache (VarObj-cache obj)))
    (for/hasheq ([(k v) (get obj)])
      (values k (hash-ref! cache k v)))))

(define* (get-all-fields! obj)
  (get-all! obj VarObj-fields))

(define* (get-all-attrs! obj)
  (get-all! obj VarObj-attrs))

(define* (get-all-axioms obj)
  (VarObj-axioms obj))

(define* (sort-hash-by-key h)
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
