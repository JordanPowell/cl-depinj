;;;; package.lisp

(defpackage #:cl-depinj
  (:use #:cl)
  (:export #:define-class-injection
	   #:define-object-injection 
	   #:with-temporary-injections
	   #:inject
	   #:enable-inject-character
	   #:disable-inject-character))

