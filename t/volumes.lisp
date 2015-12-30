(in-package :cl-cnn-test)

(plan nil)

(diag "Volume sans valeur initialle")
(let ((vol (make-instance 'volume :sx 2 :sy 3 :depth 4)))
  (ok vol)
  (is (slot-value vol 'cnn::sx) 2)
  (is (slot-value vol 'cnn::sy) 3)
  (is (slot-value vol 'cnn::depth) 4)

  (is (array-dimension (slot-value vol 'cnn::w) 0) (* 2 3 4) "Volume allocation size")
  (is (array-dimension (slot-value vol 'cnn::dw) 0) (* 2 3 4) "Volume allocation size")
  (isnt (+ (aref (slot-value vol 'cnn::w) 0) (aref (slot-value vol 'cnn::w) 1)) 0 "Random init")
  )

(diag "Volume avec valeur initialle")
(let ((vol (make-instance 'volume :sx 2 :sy 3 :depth 4 :c 10.0)))
  (ok vol)
  (isnt (+ (aref (slot-value vol 'cnn::w) 0) (aref (slot-value vol 'cnn::w) 1)) 20 "init avec C")
  )


(finalize)
