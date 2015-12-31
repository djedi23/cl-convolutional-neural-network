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
			:components
			((:file "volumes" :depends-on ("package"))
			 (:file "net" :depends-on ("package"))
			 (:file "cl-cnn")
			 (:file "package")
			 (:module "layers"
				  :components
				  ((:file "layer")
				   (:file "input" :depends-on ("layer"))
				   (:file "fully-connected" :depends-on ("layer"))
				   ))
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
