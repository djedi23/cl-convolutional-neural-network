(in-package :cl-user)
(defpackage cl-cnn-test
  (:use :cl
        :cl-cnn
        :prove))
(in-package :cl-cnn-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-cnn)' in your Lisp.

(plan nil)

;; blah blah blah.
(ok t)

(finalize)
