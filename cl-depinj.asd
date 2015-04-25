;;;; cl-depinj.asd

(asdf:defsystem #:cl-depinj
  :version "0.2"
  :serial t
  :license "BSD 2-Clause"
  :description "Simple dependency injection helpers for CL"
  :author "Jordan Rhys Powell"
  :components ((:file "package")
               (:file "cl-depinj")))

(asdf:defsystem #:cl-depinj-tests
  :version "0.2"
  :serial t
  :license "BSD 2-Clause"
  :description "Tests for cl-depinj"
  :author "Jordan Rhys Powell"
  :depends-on (#:cl-depinj #:clunit)
  :components ((:module tests
			:components ((:file "package")
				     (:file "tests")))))
