#lang racket/base

#|

This DSL type thing makes it easier to declare simple variant
specifications.

Example:

#lang s-exp "konffaa/single-variant.rkt"
<requires and other module-level declarations>
(super-is symbian-variant%) OR (super-from "symbian.var.rkt")
<attributes and other class members>

|#

(require racket "util.rkt" "variant.rkt")

(provide (rename-out (module-begin #%module-begin))
         (except-out (all-from-out racket) #%module-begin)
         (all-from-out "util.rkt" "variant.rkt"))

(define-syntax (module-begin stx)
  (syntax-case stx ()
      ((_ body ...)
       (let ((super-info #f)
             (preamble-lst '()))
         (let loop ((ml-lst (syntax->list #'(body ...))))
           (cond
            (super-info
             #`(#%module-begin
                #,@(reverse preamble-lst)
                #,@(let ((spec (car super-info)))
                     (if (not spec)
                         '()
                         (list #`(require #,spec))))
                (define-variant*/default
                  #,(cdr super-info)
                  (super-new)
                  #,@ml-lst)))
            ((null? ml-lst)
             (error 'single-variant
                    "missing super-is or super-from"))
            (else
             (let ((ml (car ml-lst)))
               (syntax-case ml (super-is super-from)
                 ((super-is id)
                  (identifier? #'id)
                  (set! super-info (cons #f #'id)))
                 ((super-from spec)
                  (let* ((klass (datum->syntax stx 'klass%))
                         (parent (datum->syntax stx 'parent%))
                         (req #`(only-in spec (#,klass #,parent))))
                    (set! super-info (cons req parent))))
                 (_
                  (set! preamble-lst (cons ml preamble-lst))))
               (loop (cdr ml-lst))))))))))
