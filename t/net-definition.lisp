(in-package :cl-cnn-test)

(plan nil)

(define-net example
    (input :out-sx 1 :out-sy 1 :out-depth 2)
  (svm :name "toto")
  (fully-connected :num-classes 2)
  )


(finalize)
