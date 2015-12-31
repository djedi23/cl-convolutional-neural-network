(in-package :cl-cnn-test)

(plan nil)

(diag "Input Layer")
(let ((net (make-instance 'net)))
  (ok net)
  (is (slot-value net 'cnn::layers) '())
  )

(finalize)
