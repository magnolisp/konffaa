#lang konffaa

#|

|#

(define-variant Project ()
  (define-attribute platform '(cxx))
  (define-attribute symbian? (true? (memq 'symbian platform)))
  (define-attribute s60? (true? (memq 's60 platform))))

(define-variant Symbian (Project)
  (define-attribute platform '(cxx symbian)))

(define-variant** S60 (Symbian)
  (define-attribute platform '(cxx symbian s60))
  (define-attribute s60-vernum 32)
  (define-attribute kit-vernum 31)

  (define-axiom s60-vernum-range
    (assert (>= s60-vernum #d09)))
  
  ;; Music Player Remote Control API (S60)
  (define-attribute have-mplayerremotecontrol
    (and (<= 31 s60-vernum 32)
         (= kit-vernum 31))))

;;(class-all-attrs S60)
