(in-package #:cl-cnn)
(declaim (optimize (debug 3)))

(defmacro constructor (name &body body)
  `(defun ,name (&rest rest)
     ,(format nil "Constructor for the class ~a" name)
     (apply #'make-instance ',name rest)))
