(in-package :cl-cnn-test)

(plan nil)

#+not-working
(cnn::define-net example
    (input :out-sx 1 :out-sy 1 :out-depth 2)
  (cnn::svm :name "toto")
  (fully-connected :num-classes 2)
  )


(finalize)
