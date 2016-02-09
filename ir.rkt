#lang racket/base

#|

Defines an internal representation for configuration information.

|#

(require "util.rkt" racket/set)

(define-struct* Field (func)
  #:property prop:procedure 0)

(define-syntax-rule* (field-thunk e ...)
  (Field (lambda () e ...)))

(define-struct* VarCls
  (name ;; variant name
   bases ;; base classes
   ctor) ;; constructor function (not for bases)
  #:transparent)

(define-struct* VarObj
  (cls ;; variant class
   name ;; file basename or class name
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
  (define obj (VarObj cls name (make-hasheq)
                      (make-hasheq) (make-hasheq)))
  (run-ctors cls obj)
  (unless (hash-empty? (VarObj-cache obj))
    (error 'make-VarObj "unexpected early cache updates detected"))
  obj)

(define* (has-attr? obj name)
  (hash-has-key? (VarObj-attrs obj) name))

(define (not-found kind obj name)
  (define cls (VarObj-cls obj))
  (define vname (VarCls-name cls))
  (error 'not-found "no ~a ~a for variant ~a"
         kind name (VarObj-name obj)))

(define* (get-attr! obj name
                    [nf (lambda () (not-found "attribute" obj name))])
  (define attrs (VarObj-attrs obj))
  (cond
    [(not (hash-has-key? attrs name))
     (nf)]
    [else
     (let ((cache (VarObj-cache obj)))
       (hash-ref! cache name
                  (lambda ()
                    (define v (hash-ref attrs name))
                    (v))))]))

(define* (get-all-attrs! obj)
  (let ((cache (VarObj-cache obj)))
    (for/hasheq ([(k v) (VarObj-attrs obj)]
                 #:unless (Field? v))
      (values k (hash-ref! cache k v)))))

(define* (get-all-axioms obj)
  (VarObj-axioms obj))

(define* (sort-hash-by-key h)
  (let* ((lst (hash-map h (lambda (k v) (list k v)))))
    (sort lst symbol<? #:key car)))
