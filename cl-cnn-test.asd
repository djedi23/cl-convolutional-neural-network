#|
  This file is a part of cl-cnn project.
|#

(in-package :cl-user)
(defpackage cl-cnn-test-asd
  (:use :cl :asdf))
(in-package :cl-cnn-test-asd)

(defsystem cl-cnn-test
  :author ""
  :license ""
  :depends-on (:cl-cnn
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "cl-cnn")
		 (:test-file "volumes"))))
  :description "Test system for cl-cnn"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
