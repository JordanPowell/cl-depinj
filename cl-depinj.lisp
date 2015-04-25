;;;; cl-depinj.lisp

(in-package #:cl-depinj)

(defparameter *injection-table* (make-hash-table))

(defun add-to-injection-table (key-symbol value)
  (setf (gethash key-symbol *injection-table*) value))

(defun add-class-injection (symbol class-symbol)
  (add-to-injection-table symbol (cons :class class-symbol)))

(defun add-object-injection (symbol object)
  (add-to-injection-table symbol (cons :object object)))

(defun get-injection (symbol)
  (gethash symbol *injection-table*))


(defun is-class-injection (injection)
  (eq :class (car injection)))

(defun is-object-injection (injection)
  (eq :object (car injection)))

(defun get-instance-from-injection (injection &rest args)
  (apply #'make-instance (cdr injection) args))

(defun get-object-from-injection (injection)
  (cdr injection))


(defun inject (symbol &rest initargs)
  "Return the injection value for symbol, or nil if one hasn't been defined. See DEFINE-CLASS-INJECTION and DEFINE-OBJECT-INJECTION."
  (let ((injection (get-injection symbol)))
    (cond ((is-class-injection injection)
	   (apply #'get-instance-from-injection injection initargs))
	  ((is-object-injection injection)
	   (get-object-from-injection injection))
	  ((null injection) nil)
	  (t (error "Bug - unusual replacement in injection table: ~a" injection)))))


(defun define-class-injection (symbol class-symbol)
  "Indicate that instances of CLASS-SYMBOL should be created when SYMBOL is injected"
  (add-class-injection symbol class-symbol))

(defun define-object-injection (symbol object)
  "Indicate that OBJECT should be returned when SYMBOL is injected"
  (add-object-injection symbol object))

(defmacro with-temporary-injections (&body body)
  "Execute BODY with a temporary copy of the global injection table. Any injections defined in BODY will be lost on exiting the block."
  (let ((old-table (gensym)))
    `(let ((,old-table (copy-hash-table *injection-table*)))
       (unwind-protect (progn ,@body)
	 (setf *injection-table* ,old-table)))))


(defvar *readtable-stack* nil)

(defun inject-read-macro (stream char)
  (declare (ignore char))
  `(inject ',(read stream)))

(defmacro enable-inject-character (&optional (character #\@))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (push *readtable* *readtable-stack*)
     (setq *readtable* (copy-readtable))
     (set-macro-character ,character 'inject-read-macro)))

(defmacro disable-inject-character ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (setq *readtable* (pop *readtable-stack*))))


;; Copied from alexandria@b1c6ee03c41e0db97989ae38e70da4d8263e09d1
(defun copy-hash-table (table &key key test size
				rehash-size rehash-threshold)
  "Returns a copy of hash table TABLE, with the same keys and values
as the TABLE. The copy has the same properties as the original, unless
overridden by the keyword arguments.

Before each of the original values is set into the new hash-table, KEY
is invoked on the value. As KEY defaults to CL:IDENTITY, a shallow
copy is returned by default."
  (setf key (or key 'identity))
  (setf test (or test (hash-table-test table)))
  (setf size (or size (hash-table-size table)))
  (setf rehash-size (or rehash-size (hash-table-rehash-size table)))
  (setf rehash-threshold (or rehash-threshold (hash-table-rehash-threshold table)))
  (let ((copy (make-hash-table :test test :size size
                               :rehash-size rehash-size
                               :rehash-threshold rehash-threshold)))
    (maphash (lambda (k v)
               (setf (gethash k copy) (funcall key v)))
             table)
    copy))
