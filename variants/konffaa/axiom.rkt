#lang racket

#|

At the lowest levels of abstraction you may use (call-method object
method-name) to test an axiom. We also build a basic testing facility
on top. We do not use 'rackunit' for testing, as it does not appear
designed to deal with tests as values, which we may want to do here.
We also do not require "expect" type statements, as configuration
axioms deal with relations.

In some cases, instead of axioms, you may want to simply declare
attribute getters as final to ensure they always have a certain kind
of value for a given subset of configurations. Then already any
attempt to override will cause an error, even before we get to
testing.

|#

(require "util.rkt")

;; symbol -> symbol
(define (to-method-name axiom-name)
  (string->symbol
   (string-append (symbol->string axiom-name) ".axiom")))

(define axiom-method-name-re #rx"^(.*)[.]axiom$")

;; object -> [(axiom-name method-name)]
(define (object-axioms/list object)
  (let ((mnames (interface->method-names (object-interface object))))
    (for/fold
     ((res '()))
     ((mname mnames))
     (if-let m (regexp-match axiom-method-name-re (symbol->string mname))
             (cons (list (string->symbol (second m)) mname) res)
             res))))

;; object -> [(axiom-name method-name)]
(define (object-axioms/list/sorted object)
  (define lst (object-axioms/list object))
  (sort lst symbol<? #:key first))

;; object -> {axiom-name : method-name}
(define (object-axioms/hash object)
  (foldl
   (lambda (x res)
     (define k (first x))
     (define v (second x))
     (hash-set res k v))
   #hasheq()
   (object-axioms/list object)))

;;; 
;;; testing
;;; 

(struct	assertion-failed (f m) #:transparent)

(define (check-not-false expr form msg)
  (unless expr
    (raise (assertion-failed form msg))))

(define-syntax* assert
  (syntax-rules ()
    ((_ e)
     (assert e #f))
    ((_ e msg)
     (check-not-false e (quote e) msg))))

(define-syntax-rule* (implies x y)
  (or (not x) y))

(define-syntax-rule* (iff x y)
  (or (and x y)
      (not (or x y))))

(define* (run-axiom-based-tests object suite-desc)
  (define axioms (object-axioms/list/sorted object))
  (define num-all (length axioms))
  (define num-failed 0)
  (for-each
   (lambda (x)
     (define an (symbol->string (first x)))
     (define mn (second x))
     (with-handlers
         ((assertion-failed?
           (lambda (ex)
             (set! num-failed (+ num-failed 1))
             (printfln "failed test")
             (printfln "      axiom: ~a" an)
             (printfln "  assertion: ~a"
                       (or
                        (assertion-failed-m ex)
                        (assertion-failed-f ex))))))
       (call-method object mn)))
   axioms)
  (when (> num-failed 0)
    (printfln "there are ~a (of ~a) untrue axioms in ~a"
              num-failed num-all suite-desc)
    (error "aborted"))
  (printfln "all ~a axioms hold for ~a" num-all suite-desc)
  (void))

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
