#lang racket

#|

|#

(require konffaa/variant)

(define-variant project-variant%
  variant%
  (super-new)
  (define/public (platform) '(cxx))
  (define/public (symbian?) (memq 'symbian (platform)))
  (define/public (s60?) (memq 's60 (platform))))

(define-variant s60-variant%
  project-variant%
  (super-new)
  (define/override (platform) '(cxx symbian s60)))

(define-variant*/default
  s60-variant%
  (super-new)

  (inherit s60?)
  
  (introduce-attr s60-vernum 32)
  (introduce-attr kit-vernum 31)

  (declare-attr s60-vernum kit-vernum)
  
  ;; Music Player Remote Control API (S60)
  (introduce-attr have-mplayerremotecontrol
    (and (<= 31 s60-vernum 32)
         (= kit-vernum 31))))
