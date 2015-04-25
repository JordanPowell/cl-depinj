cl-depinj
=========

A few 'dependency injection' helpers for CL


Dependencies
============

[clunit](https://github.com/tgutu/clunit) if you want to run the tests


Documentation
=============

cl-depinj is basically a glorified global variable table with a handy read macro for easy access. It came about because I wanted to use different concrete instances/objects in different installations of my application and I was bored of lots of this:

    ;; config.lisp:
    (defparameter *blah-blah-class-name* 'FooClass)

    ;; everywhere else:
    (make-instance *blah-blah-class-name*)


I'm not sure if it really is dependency injection (or what that even means in a language as dynamic as lisp) but it feels nicer than what I had before.

There are two types of injections supported. 'Object injections' simply return the given object when injected and 'class injections' return a new instance of the given class when injected.


Example Usage
-------------

First, object injections:

    ;; Define an object injections:
    > (define-object-injection 'favourite-film "The Matrix")

    ;; Use it in a function
    > (defun print-favourite-film ()
        (format t "My favourite film is ~a!~%" (inject 'favourite-film)))

    ;; See it working:
    > (print-favourite-film)
    My favourite film is The Matrix!
    NIL

    ;; Change the injection
    > (define-object-injection 'favourite-film "Sunshine")

    ;; See how it's changed:
    > (print-favourite-film)
    My favourite film is Sunshine!
    NIL

Pretty basic stuff. Now for class injections:

    ;; Define a class
    > (defclass HappyPrinter ()
        ((salutation :initarg :salutation :initform "Happy days!" :reader salutation)))
    > (defun print-message (printer msg)
        (format t "~a ~a~%" (salutation printer) msg))

    ;; Define the injection
    > (define-class-injection 'Printer 'HappyPrinter)

    ;; See it working
    > (print-message (inject 'Printer) "Hello")
    Happy days! Hello

    ;; Try passing an argument
    > (print-message (inject 'Printer :salutation "Yay!") "Hello")
    Yay! Hello

The read macro:

    > (enable-inject-character)

    > (define-object-injection 'mood "thrilled")

    > (print-message @Printer (format nil "I'm ~a by this library" @mood))
    Happy days! I'm thrilled by this library

    > (disable-inject-character)


Finally, you can establish temporary injections with `with-temporary-injections` (useful for tests):

    > (enable-inject-character)

    > (define-object-injection 'sort-predicate #'>)

    > (sort '(1 3 2) @sort-predicate)
    (3 2 1)

    > (with-temporary-injections
        (define-object-injection 'sort-predicate #'<)
        (sort '(1 3 2) @sort-predicate))
    (1 2 3)

    > (sort '(1 3 2) @sort-predicate)
    (3 2 1)

    > (disable-inject-character)

Running the tests
-----------------

If you've got [clunit](https://github.com/tgutu/clunit) installed, once you load `cl-depinj-tests` you should just be able to `(cl-depinj-tests:run-all-tests)`


Why should I use it?
====================

Perhaps you shouldn't. I think we can all agree that overuse of global variables is a bad thing and this library makes it even easier to overuse them. I think there is a case for having a config file in a large project to set up the appropriate classes for the installation, and for being able to use different implementations in test code or while debugging, e.g.

    (with-temporary-injections
      (define-class-injection 'SMSService 'DummySMSService)
      ; test some code that normally sends sms messages)

I'm still not sold that a library like this is a good idea but I can't think of a better alternative for my projects. If you want a long and boring technical discussion about it feel free to contact me - I like long and boring technical discussions.


Is it safe? Will it work for me?
================================

Maybe. It's working for me so far.


The Latest Version
==================

To the extent that I'm using versioning, this is version `0.2`. If I ever need to create another version, I promise to follow [Semantic Versioning 2.0.0](http://semver.org/spec/v2.0.0.html).


Installation
============

I just use `(asdf:load-system :cl-depinj)` - YMMV.


Licensing
=========

Please see the LICENSE file


Contact
=======

Github issues and pull-requests are the preferred method. If you live in Brighton and want to buy me a beer go ahead, you'll get a special mention here (remind me that it's a contribution to cl-depinj):

Beer contributors: `'()`
