#lang info
(define name "Konffaa")
(define blurb '("A configuration manager."))
(define racket-launcher-libraries '("konffaa-cli.rkt"))
(define racket-launcher-names '("konffaa"))
(define compile-omit-paths '("gh-pages" "retired" "variants"))
(define deps '(("base" #:version "6.3")))