(in-package :cl-cnn-test)

(plan nil)

(diag "Fully connected Layer")

(let ((fc (make-instance 'fully-connected :sx 1 :sy 2 :depth 3 :num-neurons 20)))
  (ok fc)
  (is (cnn::num-inputs fc) 6)
  (is (class-name (class-of (slot-value fc 'cnn::biases))) 'VOLUME)
  (is (value (slot-value fc 'cnn::biases) 1 1 19) 0.0)
  (with-slots (cnn::filters) fc
    (is (array-dimension cnn::filters 0) 20)
    (is (cnn::depth (aref cnn::filters 0)) 6)
    (isnt (aref cnn::filters 0) (aref cnn::filters 1))
    ))

(let ((fc (make-instance 'fully-connected :sx 1 :sy 2 :depth 3 :num-neurons 20 :bias 3.0)))
  (ok fc)
  (is (cnn::num-inputs fc) 6)

  (is (class-name (class-of (slot-value fc 'cnn::biases))) 'VOLUME)
  (is (value (slot-value fc 'cnn::biases) 1 1 19) 3.0)
  )

(diag "Fully connected forward")
(let ((fc (make-instance 'fully-connected :sx 1 :sy 2 :depth 3 :num-neurons 20 :bias 3.0)))
  (forward fc (make-instance 'volume :sx 1 :sy 2 :depth 10 :c 10.0))
  (is (cnn::depth (cnn::in-act fc)) 10)
  (is (cnn::depth (cnn::out-act fc)) 20)
  )


(finalize)
