#lang info
(define name "Konffaa")
(define blurb '("A configuration manager."))
(define scribblings '(("manual-src/konffaa.scrbl" ())))
(define racket-launcher-libraries '("konffaa-cli.rkt"))
(define racket-launcher-names '("konffaa"))
(define compile-omit-paths '("gh-pages" "retired" "src" "variants"))
(define deps '(("base" #:version "6.3")))
(define build-deps '("at-exp-lib" "racket-doc" "scribble-lib"))
