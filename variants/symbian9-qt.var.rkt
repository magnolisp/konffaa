#lang racket

#|

Self-signed release build for Symbian v9 and higher.
Uses Qt Mobility to read contact data.
Creates a console to show possible error output.

|#

(require "base.rkt")
(require "konffaa/util.rkt")
(require "konffaa/variant.rkt")

(define-variant*/default symbian-variant%
  (super-new)

  (define/override (feature-qt-contacts.attr)
    #t)

  ;; Without sufficient capabilities we simply get a KERN-EXEC 3
  ;; from the Qt Mobility Contacts API. How nice. At least
  ;; WriteUserData is not required, which achieves the principle of
  ;; least privilege, unlike on Harmattan.
  (define/override (capabilities.attr)
    '(ReadUserData))
  
  ) ;; end class

