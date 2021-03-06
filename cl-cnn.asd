#|
  This file is a part of cl-cnn project.
|#

(in-package :cl-user)
(defpackage cl-cnn-asd
  (:use :cl :asdf))
(in-package :cl-cnn-asd)

(defsystem cl-cnn
  :version "0.1"
  :author "moise.valvassori"
  :license ""
  :depends-on (:infix)
  :components ((:module "src"
			:serial t
			:components
			((:file "package")
			 (:file "utils" :depends-on ("package"))
			 (:file "volumes" :depends-on ("utils"))
			 (:file "cl-cnn")
			 (:module "layers"
				  :components
				  ((:file "layer")
				   (:file "loss" :depends-on ("layer"))
				   (:file "activation" :depends-on ("layer"))
				   (:file "input" :depends-on ("layer"))
				   (:file "fully-connected" :depends-on ("layer"))
				   (:file "convolutional" :depends-on ("layer"))
				   (:file "relu" :depends-on ("activation"))
				   (:file "sigmoid" :depends-on ("activation"))
				   (:file "softmax" :depends-on ("loss"))
				   (:file "svm" :depends-on ("loss"))
				   (:file "dropout" :depends-on ("layer"))
				   ))
			 (:file "net" :depends-on ("utils"))
			 (:file "net-definition" :depends-on ("net"))
			 )))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op cl-cnn-test))))
