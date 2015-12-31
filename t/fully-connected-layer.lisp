(in-package :cl-cnn-test)

(plan nil)

(diag "Fully connected Layer")

(let ((fc (make-instance 'fully-connected :sx 1 :sy 2 :depth 3 :num-neurons 20)))
  (ok fc)
  (is (cnn::num-inputs fc) 6)
  )


(finalize)
