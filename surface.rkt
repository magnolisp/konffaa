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

(define-syntax-rule* ($ name)
  (get-attr! self 'name))

(define-syntax* (define-field stx)
  (syntax-parse stx
    [(_ name:id v:expr ...+)
     #'(begin
         (define-id-syntax name (get-attr! self 'name))
         (hash-set! (VarObj-attrs self) 'name (field-thunk v ...)))]))

(define-syntax* (define-attribute stx)
  (syntax-parse stx
    [(_ name:id v:expr ...+)
     #'(begin
         (define-id-syntax name (get-attr! self 'name))
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

(define* (class-all-attrs cls)
  (define obj
    (make-VarObj cls))
  (get-all-attrs! obj))
