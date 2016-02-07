#lang racket

#|

Defines an internal representation for configuration information.

|#

(require "util.rkt")

(define-struct* Variant
  (name ;; variant name
   bases ;; base variants, in order
   attrs ;; attributes (values or functions)
   axioms ;; axioms (functions)
   cache) ;; attribute values
  #:transparent)

(define* (make-Variant name bases)
  (Variant name bases (make-hasheq) (make-hasheq) (make-hasheq)))

(struct NF ()) ;; not found

(define (lookup vrnt name)
  (define attrs (Variant-attrs vrnt))
  (cond
    [(hash-has-key? attrs name)
     (hash-ref attrs name)]
    [else
     (let loop ((bases (Variant-bases vrnt)))
       (cond
         [(null? bases) (NF)]
         [else
          (let ((base (car bases))
                (bases (cdr bases)))
            (define val (lookup base name))
            (cond
              [(not (NF? val))
               val]
              [else
               (loop bases)]))]))]))

(define (default-nf vrnt name)
  (error 'get-attr! "no attribute ~a for variant ~a"
         name (Variant-name vrnt)))

(define (postprocess-attr val)
  (if (procedure? val)
      (val)
      val))

(define* (get-attr! vrnt name [not-found default-nf])
  (let ((cache (Variant-cache vrnt)))
    (cond
      [(hash-has-key? cache name)
       (hash-ref cache name)]
      [else
       (define val (lookup vrnt name))
       (cond
         [(NF? val)
          (not-found vrnt name)]
         [else
          (let ((val (postprocess-attr val)))
            (hash-set! cache name val)
            val)])])))

(define (get-all! vrnt get post cache)
  (for ([(k v) (get vrnt)])
    (unless (hash-has-key? cache k)
      (let ((v (post v)))
        (hash-set! cache k v))))
  (for ([base (Variant-bases vrnt)])
    (get-all! base get post cache))
  cache)

(define* (get-all-attrs! vrnt)
  (let ((cache (Variant-cache vrnt)))
    (get-all! vrnt Variant-attrs postprocess-attr cache)))

(define* (get-all-axioms vrnt)
  (let ((cache (make-hasheq)))
    (get-all! vrnt Variant-axioms identity cache)))

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
