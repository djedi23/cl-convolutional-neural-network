(in-package :cl-cnn-test)

(plan nil)

(diag "tutorial net")
(let ((net (make-instance 'net)))
  (ok net)
  (is (array-dimension (cnn::layers net) 0) 2)
  (is (fill-pointer (cnn::layers net)) 0)

  (add-layer net (input :out-sx 1 :out-sy 1 :out-depth 2))
  (add-layer net (fully-connected :num-neurons 20))
  (add-layer net (relu))
  (add-layer net (softmax))
  (add-layer net (fully-connected :num-neurons 10))

  (is (fill-pointer (cnn::layers net)) 5)
  (is (class-name (class-of (aref (cnn::layers net) 1))) 'FULLY-CONNECTED)
  (with-slots (cnn::in-sx cnn::in-sy cnn::in-depth
			  cnn::out-sx cnn::out-sy cnn::out-depth) (aref (cnn::layers net) 1)
    (is cnn::in-sx 1)
    (is cnn::in-sy 1)
    (is cnn::in-depth 2)
    (is cnn::out-sx 1)
    (is cnn::out-sy 1)
    (is cnn::out-depth 20)
    )

  (let ((f (forward net (volume :sx 1 :sy 1 :depth 2 :w #(0.3 -0.5)))))
    (ok (< (abs (- (aref (cnn::w f) 0) 0.1)) 0.05) (format nil "Error ~a < 0.05" (abs (- (aref (cnn::w f) 0) 0.1))))
    ))


(diag "example SVM from layer documentation")
(let ((net (make-instance 'net)))
  (ok net)
  (is (array-dimension (cnn::layers net) 0) 2)
  (is (fill-pointer (cnn::layers net)) 0)

  (add-layer net (input :out-sx 1 :out-sy 1 :out-depth 2))
  (add-layer net (svm))
  (add-layer net (fully-connected :num-classes 2))

  (is (fill-pointer (cnn::layers net)) 4)
  (is (class-name (class-of (aref (cnn::layers net) 1))) 'SVM)
  (with-slots (cnn::in-sx cnn::in-sy cnn::in-depth
			  cnn::out-sx cnn::out-sy cnn::out-depth) (aref (cnn::layers net) 1)
    (is cnn::in-sx 1)
    (is cnn::in-sy 1)
    (is cnn::in-depth 2)
    (is cnn::out-sx 1)
    (is cnn::out-sy 1)
    (is cnn::out-depth 2)
    )

  (let ((f (forward net (volume :sx 1 :sy 1 :depth 2 :w #(0.5 -1.3)))))
    (ok (< (abs (- (aref (cnn::w f) 0) 0.1)) 0.8) (format nil "Error ~a < 0.8" (abs (- (aref (cnn::w f) 0) 0.1))))
    ))


(finalize)
