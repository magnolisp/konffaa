#lang racket/base

#|
|#

(require (for-syntax racket/base)
         racket/function
         racket/list
         racket/match
         racket/port
         racket/string
         racket/unit
         "attribute.rkt"
         "util.rkt")

;;;
;;; generic utilities
;;;

(define-syntax on-fail
  (syntax-rules ()
    ((_ fail-expr expr)
     (with-handlers
         ((exn:fail?
           (lambda (e) fail-expr)))
       expr))))

(define (file-read file)
  (call-with-input-file*
   file
   (lambda (in)
     (port->string in))))
     
;; Checks whether a file either does not exist or has been changed.
(define (file-changed? file s)
  ;; Would there be a good way to write a function for comparing two
  ;; input streams? Then we could handle large files as well. ((nin
  ;; (open-input-string s))) and then compare to file input.
  (on-fail #t (not (equal? (file-read file) s))))

;;(write-nl (file-changed? configure-script (file-read configure-script)))

(define* (write-changed-file file s)
  (when (file-changed? file s)
    (call-with-output-file*
     file
     (lambda (out)
       (display s out))
     #:exists 'truncate/replace)
    (displayln file)))

(define (capture-output f)
  (let ((output (open-output-string)))
    (parameterize ((current-output-port output))
      (f))
    (get-output-string output)))

(define-syntax* capture
  (syntax-rules ()
    ((_ body ...)
     (capture-output (lambda () body ...)))))

(define* (space-join l)
  (string-join l " "))

(define (for-each-sep elemact sepact lst)
  (define first #t)
  (for-each
   (lambda (elem)
     (if first
         (set! first #f)
         (when sepact (sepact)))
     (when elemact (elemact elem)))
   lst))

;;;
;;; local utilities
;;;

(define (disp . args)
  (display (apply format args)))

(define (disp-nl . args)
  (apply disp args) (newline))

;;;
;;; pretty printing
;;;

(define* (display-generated-notice pfx)
  (display pfx)
  (displayln " generated -- do not edit"))

(define* (write-scheme-symlink file target)
  (write-changed-file
   file
   (capture
    (display-generated-notice ";;")
    (displayln "#lang racket")
    (disp-nl "(require ~s)" target)
    (disp-nl "(provide (all-from-out ~s))" target))))

(define path-censor-re #rx"[-.]")

(define* (path-h-ifdefy p)
  (string-append
   "__"
   (string-downcase
    (regexp-replace* path-censor-re (path->string (path-basename p)) "_"))
   "__"
   ))

(define (ident-sanitize a-s)
  (define s a-s)
  (set! s (regexp-replace* #rx"^[-]" s "_"))
  (when-let r (regexp-match #rx"^(.*)[?]$" s)
    (set! s (string-append "is_" (second r))))
  (set! s (regexp-replace* #rx"^[^a-zA-Z_]+" s ""))
  (set! s (regexp-replace* #rx"[^a-zA-Z0-9_]+" s ""))
  (when (string=? s "")
    (raise-argument-error 'ident-sanitize "emittable name" a-s))
  s)

(define* (name-to-c sym)
  (string->symbol
   (string-upcase
    (ident-sanitize (symbol->string sym)))))

(define* (name-to-ruby sym)
  (string->symbol
   (string-append
    "$"
    (string-upcase
     (ident-sanitize (symbol->string sym))))))

(define* (name-to-gmake sym)
  (name-to-c sym))

(define* (name-to-gmake/negate sym)
  (string->symbol
   (string-append
    "NOT__"
    (symbol->string (name-to-gmake sym)))))

(define (bool-attr? attr)
  (boolean? (second attr)))

(define (true-attr? attr)
  (eqv? #t (second attr)))

;; Returns a list of symbols.
(define (bool-attrs-to-qmake-list attrs)
  (map
   (lambda (entry)
     (let ((name (first entry)))
       (name-to-gmake name)))
   (filter true-attr? attrs)))

;; Returns a list of symbols.
(define (bool-attrs-to-qmake-list/with-negates attrs)
  (map
   (lambda (entry)
     (let ((name (first entry))
           (value (second entry)))
       ((if value name-to-gmake name-to-gmake/negate) name)))
   (filter bool-attr? attrs)))

(define* (display/c value)
  (cond
   ((or (attr-defined? value) (eqv? value #t))
    (display "1"))
   ((eqv? value #f)
    (display "0"))
   ((number? value)
    (write value))
   ((hexnum? value)
    (display (format "0x~a" (number->string (hexnum-num value) 16))))
   ((string? value)
    (write value))
   ((symbol? value)
    (display/c (symbol->string value)))
   ((list? value)
    (begin
      (display "{")
      (for-each-sep display/c (thunk (display ", ")) value)
      (display "}")))
   (else
    (error "cannot display as C" value))
   ))

(define* (display-attr/c name value)
  (cond
   ((attr-undefined? value)
    (void))
   (else
    (begin
      (disp "#define ~a " (name-to-c name))
      (display/c value)
      (newline)))
   ))

(define* (display/ruby value)
  (cond
   ((or (attr-defined? value) (eqv? value #t))
    (display "true"))
   ((eqv? value #f)
    (display "false"))
   ((number? value)
    (write value))
   ((hexnum? value)
    (disp "0x~a" (number->string (hexnum-num value) 16)))
   ((string? value)
    (write value))
   ((symbol? value)
    (display/ruby (symbol->string value)))
   ((list? value)
    (begin
      (display "[")
      (for-each-sep display/c (thunk (display ", ")) value)
      (display "]")))
   (else
    (error "cannot display as Ruby" value))
   ))

(define* (display-attr/ruby name value)
  (cond
   ((attr-undefined? value)
    (void))
   (else
    (begin
      (display (name-to-ruby name))
      (display " = ")
      (display/ruby value)
      (newline)))
   ))

(define* (display/gmake value)
  (cond
   ((or (attr-defined? value) (eqv? value #t))
    (display "true"))
   ((number? value)
    (write value))
   ((string? value)
    (display value))
   ((symbol? value)
    (display/gmake (symbol->string value)))
   ((list? value)
    (for-each-sep display/gmake (thunk (display " ")) value))
   (else
    (error "cannot display as GNU Make" value))
   ))

(define* (display-attr/gmake name value)
  (set! name (name-to-gmake name))
  (cond
   ((attr-undefined? value)
    (void))
   ((eqv? value #t)
    (begin (disp-nl "~a := true" name)
           (disp-nl "NOT__~a :=" name)))
   ((eqv? value #f)
    (begin (disp-nl "~a :=" name)
           (disp-nl "NOT__~a := true" name)))
   ((hexnum? value)
    (begin
      (disp-nl "~a__DEC := ~s" name (hexnum-num value))
      (disp-nl "~a__HEX := ~a"
               name (number->string (hexnum-num value) 16))))
   (else
    (begin
      (display name)
      (display " := ")
      (display/gmake value)
      (newline)))
   ))

(define* (display-attr/qmake name value)
  (set! name (name-to-gmake name))
  (cond
   ((attr-undefined? value)
    (void))
   ((eqv? value #t)
    (begin (disp-nl "~a = true" name)
           ;;(disp-nl "NOT__~a =" name)
           ))
   ((eqv? value #f)
    (begin (disp-nl "~a =" name)
           ;;(disp-nl "NOT__~a = true" name)
           ))
   ((hexnum? value)
    (begin
      (disp-nl "~a__DEC = ~s" name (hexnum-num value))
      (disp-nl "~a__HEX = ~a"
               name (number->string (hexnum-num value) 16))))
   (else
    (begin
      (display name)
      (display " = ")
      (display/gmake value)
      (newline)))
   ))

;; For convenience, we add all boolean variables (or their
;; negations) to the CONFIG variable with the += operator.
(define (display-qmake-bools attrs)
  (display "CONFIG += ")
  (for-each-sep display (thunk (display " "))
                (bool-attrs-to-qmake-list attrs))
  (newline))

;;; 
;;; generic API
;;; 

;; To define a new back end, implement the desired `lang^` as a unit,
;; and then call `make-write-include-file` to get a code generation
;; routine for the specified language.
(provide (struct-out Model) lang^)

(struct Model
  (path ; path-string?
   attrs ; (listof (list/c symbol? any/c))
   ) #:transparent)

(define-signature writer^
  (write-file ; (-> Model? any/c)
   ))

(define-signature lang^
  (line-comment-prefix ; string?
   display-attr ; (-> symbol? any/c any/c)
   display-preamble ; (-> Model? any/c)
   display-postamble ; (-> Model? any/c)
   ))

(define-unit ruby-lang@
  (import)
  (export lang^)
  (define line-comment-prefix "#")
  (define display-attr display-attr/ruby)
  (define display-preamble void)
  (define display-postamble void))

(define-unit gmake-lang@
  (import)
  (export lang^)
  (define line-comment-prefix "#")
  (define display-attr display-attr/gmake)
  (define display-preamble void)
  (define display-postamble void))

(define-unit qmake-lang@
  (import)
  (export lang^)
  (define line-comment-prefix "#")
  (define display-attr display-attr/qmake)
  (define display-preamble void)
  (define (display-postamble m)
    (display-qmake-bools (Model-attrs m))))

(define-unit c-lang@
  (import)
  (export lang^)
  (define line-comment-prefix "//")
  (define display-attr display-attr/c)
  (define (display-preamble m)
    (define harness-name (path-h-ifdefy (Model-path m)))
    (disp-nl "#ifndef ~a" harness-name)
    (disp-nl "#define ~a" harness-name))
  (define (display-postamble m)
    (define harness-name (path-h-ifdefy (Model-path m)))
    (disp-nl "#endif // ~a" harness-name)))

(define-unit include-file-writer@
  (import lang^)
  (export writer^)

  (define (write-file m)
    (write-changed-file
     (Model-path m)
     (capture
      (display-file m))))

  (define (display-file m)
    (display-generated-notice line-comment-prefix)
    (display-preamble m)
    (display-attrs m)
    (display-postamble m))

  (define (display-attrs m)
    (for ([entry (in-list (Model-attrs m))])
      (match-define (list name value) entry)
      (display-attr name value))))

(define* (make-write-include-file lang@)
  (define-compound-unit writer@
    (import)
    (export (tag writer^ W))
    (link
     [([L : lang^]) lang@]
     [([W : writer^]) include-file-writer@ L]))
  (define-values/invoke-unit/infer writer@)
  (lambda (pn attrs)
    (write-file (Model pn attrs))))

(define* write-ruby-file
  (make-write-include-file ruby-lang@))

(define* write-gmake-file
  (make-write-include-file gmake-lang@))

(define* write-qmake-file
  (make-write-include-file qmake-lang@))

(define* write-c-file
  (make-write-include-file c-lang@))

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
