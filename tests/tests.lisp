;;;; tests.lisp

(in-package #:cl-depinj-tests)

(defsuite CLDepinjSuite ())

(defmacro def-cldepinj-test (name suite &rest body)
  `(deftest ,name ,suite
     (with-temporary-injections ,@body)))


(defsuite ObjectInjectionSuite (CLDepinjSuite))

(def-cldepinj-test test-simple-object-injection (ObjectInjectionSuite)
  (define-object-injection 'obj 100)
  (assert-equal 100 (inject 'obj)))

(def-cldepinj-test test-replacing-object-injections (ObjectInjectionSuite)
  (define-object-injection 'obj 100)
  (assert-equal 100 (inject 'obj))
  (define-object-injection 'obj 10)
  (assert-equal 10 (inject 'obj)))

(def-cldepinj-test test-temporary-object-injections (ObjectInjectionSuite)
  (define-object-injection 'obj 1)
  (assert-equal 1 (inject 'obj))
  (with-temporary-injections
    (define-object-injection 'obj 2)
    (assert-equal 2 (inject 'obj)))
  (assert-equal 1 (inject 'obj)))

(enable-inject-character)

(def-cldepinj-test test-read-macro-for-object-injection (ObjectInjectionSuite)
  (define-object-injection 'obj 2)
  (assert-equal 2 @obj))

(disable-inject-character)


(defsuite ClassInjectionSuite (CLDepinjSuite))

(def-cldepinj-test test-simple-class-injection (ClassInjectionSuite)
  (defclass ExampleTestClass ()
    ((val :initform 'initial-value :reader val)))
  (define-class-injection 'TestClass 'ExampleTestClass)
  (assert-equal 'initial-value (val (inject 'TestClass))))

(def-cldepinj-test test-replacing-class-injections (ClassInjectionSuite)
  (defclass FirstTestClass ()
    ((val :initform 'first-value :reader val)))
  (defclass SecondTestClass ()
    ((val :initform 'second-value :reader val)))
  (define-class-injection 'TestClass 'FirstTestClass)
  (assert-equal 'first-value (val (inject 'TestClass)))
  (define-class-injection 'TestClass 'SecondTestClass)
  (assert-equal 'second-value (val (inject 'TestClass))))

(def-cldepinj-test test-class-injection-with-args (ClassInjectionSuite)
  (defclass ExampleTestClass ()
    ((a :initform 'a-value :initarg :a :reader a)
     (b :initform 'b-value :initarg :b :reader b)
     (c :initform 'c-value :initarg :c :reader c)))
  (define-class-injection 'TestClass 'ExampleTestClass)
  (let ((instance (inject 'TestClass :a 1 :b 2 :c 3)))
    (assert-equal 1 (a instance))
    (assert-equal 2 (b instance))
    (assert-equal 3 (c instance)))
  (let ((instance (inject 'TestClass :c 1 :a 3)))
    (assert-equal 3 (a instance))
    (assert-equal 'b-value (b instance))
    (assert-equal 1 (c instance))))

(def-cldepinj-test test-temporary-class-injections (ClassInjectionSuite)
  (defclass OuterClass ()
    ((val :initform 'outer-value :reader val)))
  (defclass InnerClass ()
    ((val :initform 'inner-value :reader val)))
  (define-class-injection 'TestClass 'OuterClass)
  (assert-equal 'outer-value (val (inject 'TestClass)))
  (with-temporary-injections
    (define-class-injection 'TestClass 'InnerClass)
    (assert-equal 'inner-value (val (inject 'TestClass))))
  (assert-equal 'outer-value (val (inject 'TestClass))))

(enable-inject-character)

(def-cldepinj-test test-read-macro-for-class-injection (ObjectInjectionSuite)
  (defclass ExampleTestClass ()
    ((val :initform 'example-value :reader val)))
  (define-class-injection 'TestClass 'ExampleTestClass)
  (assert-equal 'example-value (val @TestClass)))

(disable-inject-character)


(defun run-all-tests ()
  (run-suite 'CLDepinjSuite))

(defun debug-all-tests ()
  (run-suite 'CLDepinjSuite :use-debugger t))
