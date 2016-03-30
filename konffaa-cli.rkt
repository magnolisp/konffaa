#lang racket/base

#|

A configuration manager. Derived from ContextLogger2's Konffaile tool.

|#

(require racket/cmdline racket/list
         racket/path racket/pretty
         "axiom.rkt"
         "ir.rkt"
         "util.rkt"
         "writer.rkt")

;; --------------------------------------------------
;; variant management
;; --------------------------------------------------

(define variant-dir (build-path "variants"))

(define (get-variant-name p)
  (let-values 
      (((base name dir) (split-path p)))
    (when dir
      (error "expected file, not a directory" p))
    (define m (regexp-match #rx"^(.*)[.]var[.](?:rkt|scm|ss)$" name))
    (unless m
      (error "cannot derive variant name from path" p))
    (second m)))

(define (get-variant-basename varname)
  (format "~a.var.rkt" varname))

(define (get-variant-file varname)
  (build-path variant-dir (get-variant-basename varname)))

(define (resolve-variant x)
  (if (file-exists? x)
      (values (get-variant-name x) (build-path x))
      (values x (get-variant-file x))))

(define src-dir (build-path "src"))

(define rkt-link-file (build-path src-dir "config-spec.rkt"))
(define rkt-config-file (build-path src-dir "config.rkt"))
(define c-config-file (build-path src-dir "current_config.hrh"))
(define gmake-config-file (build-path src-dir "current_config.mk"))
(define qmake-config-file (build-path src-dir "current_config.pri"))
(define ruby-config-file (build-path src-dir "current_config.rb"))

(define (write-variant-link varfile) ;; (-> path? void?)
  (define target ;; path string relative to `rkt-link-file`
    (path->string
     (find-relative-path
      (path->complete-path src-dir)
      (path->complete-path varfile)
      #:more-than-root? #t)))
  (write-scheme-symlink rkt-link-file target))

(define (write-variant-config attrs)
  (let ((attrs (sort-hash-by-key attrs)))
    (write-rkt-file rkt-config-file attrs)
    (write-c-file c-config-file attrs)
    (write-ruby-file ruby-config-file attrs)
    (write-gmake-file gmake-config-file attrs)
    (write-qmake-file qmake-config-file attrs)))

;; --------------------------------------------------
;; main
;; --------------------------------------------------

(module* main #f
  (define vararg
    (command-line
     #:args (config_name) config_name))

  (define-values (varname varfile)
    (resolve-variant vararg))
  
  (let* ((varinfo-cls (dynamic-require
                         (path->string varfile)
                         'klass%))
         (varinfo (make-VarObj varinfo-cls #:name varname))
         (attrs (let ((x (get-all-attrs! varinfo)))
                  (hash-set x 'name varname))))
    (run-axiom-based-tests varinfo varname)

    ;;(pretty-print (list varname varinfo attrs))

    (write-variant-config attrs)
    (write-variant-link varfile)

    (void)))

#|

Copyright 2009 Helsinki Institute for Information Technology (HIIT)
and the authors. All rights reserved.

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
