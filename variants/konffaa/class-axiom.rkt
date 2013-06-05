#lang racket

#|

You may use (call-method object method-name) to test an axiom. You may
find check-not-false of rackunit useful in defining assertions within
axioms. In some cases, instead of axioms, you may want to simply
declare attribute getters as final to ensure they always have a
certain kind of value for a given subset of configurations.

The 'rackunit' module exports a 'test-case?' predicate, but quickly
glancing through the documentation could not see anything that would
allow test case objects to be constructed. Hence the use of
rackunit/private/base to create such objects directly.

|#

(require "util.rkt")
(require rackunit)
(require (only-in rackunit/private/base rackunit-test-case))

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

(define* (object->test-suite object suite-desc)
  (make-test-suite
   suite-desc
   (let ((axioms (object-axioms/list/sorted object)))
     (map
      (lambda (x)
        (define an (symbol->string (first x)))
        (define mn (second x))
        (rackunit-test-case an
                            (thunk
                             (parameterize ((current-test-name an))
                               (call-method object mn)))))
      axioms))))
;;with-check-info*

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
