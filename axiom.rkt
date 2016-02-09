#lang racket/base

#|

At the lowest levels of abstraction you may simply call an axiom to
test it. We also build a basic testing facility on top. We do not use
'rackunit' for testing, as it does not appear designed to deal with
tests as values, which we may want to do here. We also do not
require "expect" type statements, as configuration axioms deal with
relations.

|#

(require "ir.rkt" "util.rkt" racket/list)

;;; 
;;; axiom implementation
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

;; 'implies' is already defined in Racket.
(define-syntax-rule* (iff x y)
  (or (and x y)
      (not (or x y))))

;;; 
;;; axiom-based testing
;;; 

(define* (run-axiom-based-tests object suite-desc)
  (define axioms (sort-hash-by-key (get-all-axioms object)))
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
       (mn)))
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
