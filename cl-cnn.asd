#|
  This file is a part of cl-cnn project.
|#

(in-package :cl-user)
(defpackage cl-cnn-asd
  (:use :cl :asdf))
(in-package :cl-cnn-asd)

(defsystem cl-cnn
  :version "0.1"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "cl-cnn"))))
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
