;;;; package.lisp

(defpackage #:cl-depinj-tests
  (:use #:cl #:cl-depinj #:clunit)
  (:export #:run-all-tests
	   #:debug-all-tests))
