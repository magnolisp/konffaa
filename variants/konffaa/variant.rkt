#lang racket

#|

This file defines the API that a variant specification must implement.

There can be variation in feature sets, target platforms, or
toolchains, for example.

The variant specifications should strive to be fairly high-level, so
that only the actual variable choices can be made there. A
specification should pretty much be just a set of key/value pairs,
which we call attributes.

To avoid duplication in specifications, inheritance is supported.
(For this we use the built-in PLT Scheme OO facility.) Each variant
spec should be an instance of a subclass of variant%. Macros are
provided for making it easier to define such subclasses.

If you forget to invoke 'super-instantiate' or 'super-new' in you
'variant%' subclass, you will get a descriptive error message upon
instantiation attempt.

|#

(require "util.rkt")

(define-struct* hexnum (num))

(define* current-variant (make-parameter #f))

;; There is nothing here really, as a variant specification really is
;; project specific. However, a common baseclass for variants may turn
;; out to be of use.
(define* variant%
  (class object%
    (inspect #f) ;; transparent
    
    (field (name #f))
    
    (super-new)

    (define/public (variant-name.attr)
      (symbol->string name))

    ;; This can be useful for more reflective configuration setups.
    ;; Note, though, that these attributes can still be overridden
    ;; with .attr methods.
    (define/public (get-attrs)
      #hasheq())
    ))

(define-syntax* variant-class
  (syntax-rules ()
    ((_ super body ...)
     (class super
       (inspect #f)
       body ...))))

(define-syntax* define-variant
  (syntax-rules ()
    ((_ name super body ...)
     (define name
       (variant-class super body ...)))))

(define-syntax* define-variant*
  (syntax-rules ()
    ((_ name rest ...)
     (begin
       (define-variant name rest ...)
       (provide name)))))

(define-syntax* (define-variant*/default stx)
  (syntax-case stx ()
    ((_ super body ...)
     (let ((name (datum->syntax stx 'klass%)))
       #`(define-variant* #,name super body ...)))))

(begin-for-syntax
 (require racket/syntax)
 (define (make-attr-method-id ctx an-stx)
   (format-id ctx #:source an-stx
              "~a.attr" (syntax-e an-stx))))

;; We use promises to memoize, which works fine here as there are no
;; arguments. Due to hygiene, the name 'v' (to which the promise is
;; bound) cannot be referenced outside the macro.
(define-syntax-rule
  (sub-define-attr kind name e ...)
  (begin
    (define v (delay e ...))
    (define name (lambda () (force v)))
    (kind name)))

;; E.g.
;; (define-attr public feature-x #f)
;; (define-attr override feature-x #t)
;; (define-attr public-final feature-y #t)
;; (define-attr override-final feature-x #f)
(define-syntax* (define-attr stx)
  (syntax-case stx ()
    ((_ kind an e ...)
     (identifier? #'an)
     (let ((mn (make-attr-method-id stx #'an)))
       #`(sub-define-attr kind #,mn e ...)))))

;; This macro defines, for the specified attribute(s), a class local
;; assignment transformer binding that makes it possible to directly
;; use the attribute name in expressions. Order of declaration does
;; not matter, as all member names are in scope when evaluating
;; expressions giving values for members.
;;
;; E.g.
;; (define k%
;;   (class object%
;;     (super-new)
;;     (declare-attr c d)
;;     (define/public (c.attr) 6)
;;     (define/public (d.attr) (+ 1 c))
;;     ))
(define-syntax* (declare-attr stx)
  (syntax-case stx ()
    ((_ an ...)
     (let* ((an-lst (syntax->list #'(an ...)))
            (def-lst
              (map
               (lambda (an)
                 (unless (identifier? an)
                   (error 'declare-attr "expected identifier, got ~s" an))
                 (let ((mn (make-attr-method-id stx an)))
                   #`(define-syntax #,an
                       (make-set!-transformer
                        (lambda (stx)
                          (syntax-case stx ()
                            (id (identifier? #'id) #'(send this #,mn))))))))
               an-lst)))
       #`(begin #,@def-lst)))))

;; For defining alternative syntax for attribute declarations.
(define-for-syntax (make-define-attr kind-stx)
  (lambda (stx)
    (syntax-case stx ()
      ((_ an e ...)
       (identifier? #'an)
       (let ((mn (make-attr-method-id stx #'an)))
         #`(sub-define-attr #,kind-stx #,mn e ...))))))

(define-syntax* introduce-attr
  (make-define-attr #'public))

(define-syntax* override-attr
  (make-define-attr #'override))

(define-syntax* introduce-attr/final
  (make-define-attr #'public-final))

(define-syntax* override-attr/final
  (make-define-attr #'override-final))

#|

Copyright 2009-2013 Helsinki Institute for Information
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
