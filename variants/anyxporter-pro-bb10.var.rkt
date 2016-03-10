#lang konffaa

#|

|#

(define-variant Anyxporter ()
  (define-field kind #f)

  (define-axiom kind-specified
    (assert kind))
  
  (define-attribute demo? (eq? 'demo kind))
  (define-attribute lite? (eq? 'lite kind))
  (define-attribute pro? (eq? 'pro kind)))

(define-variant BB10 (Anyxporter)
  (define-attribute platform 'bb10))

(define-variant** Pro (BB10)
  (define-field kind 'pro))

;;(class-all-attrs Pro)
