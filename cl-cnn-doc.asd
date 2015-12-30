#|
  This file is a part of cl-cnn project.
|#

(in-package :cl-user)
(defpackage cl-cnn-asd
  (:use :cl :asdf))
(in-package :cl-cnn-asd)

(defsystem cl-cnn-doc
  :version "0.1"
  :author "moise.valvassori"
  :license ""
  :depends-on (#:mgl-pax #:cl-cnn)
  :components ((:module "doc"
                :components
                ((:file "doc"))))
  :description ""
  :perform (load-op :after (op c)
                    (cnn::make-readme.md)))
