(in-package #:cl-cnn)
(declaim (optimize (debug 3)))

(defmacro constructor (name &optional (var 'o) &body body)
  `(progn
     (defmethod initialize-instance :after ((,var ,name) &key &allow-other-keys)
		,@body)
     (defun ,name (&rest rest)
       ,(format nil "Constructor for the class ~a" name)
       (apply #'make-instance ',name rest))))

(defun flatten0 (x)
  "Flatten one level of nested list"
    (apply #'concatenate (cons 'list x)))