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
  (is (+ (aref (slot-value vol 'cnn::w) 0) (aref (slot-value vol 'cnn::w) 1)) 20.0 "init avec C")
  )

(diag "Construteur Volume")
(let ((vol (volume :sx 2 :sy 3 :depth 4 :c 10.0)))
  (ok vol)
  (is (+ (aref (slot-value vol 'cnn::w) 0) (aref (slot-value vol 'cnn::w) 1)) 20.0 "init avec C")
  )

(let ((vol (volume :sx 1 :sy 1 :depth 3 :w #(1234))))
  (ok vol)
  (is (array-dimension (cnn::w vol) 0) 3)
  (is (aref (cnn::w vol) 0) 1234)
  (is (aref (cnn::w vol) 1) 0)
)

(diag "Volume value")
(let ((vol (make-instance 'volume :sx 2 :sy 3 :depth 4 :c 10.0)))
  (is (value vol 1 1 2) 10.0 "Read value")
  (is (cnn::setf-value vol 1 1 2 15.0) 15.0 "set value")
  (is (value vol 1 1 2) 15.0 "Read value 2")
  (setf (value vol 1 1 2) 20.0)
  (is (value vol 1 1 2) 20.0 "Read value 3")
)

(diag "Volume add value")
(let ((vol (make-instance 'volume :sx 2 :sy 3 :depth 4 :c 10.0)))
  (is (value vol 1 1 2) 10.0 "Read value")
  (add-value vol 1 1 2 5.0)
  (is (value vol 1 1 2) 15.0 "Read added value")
)


(diag "Volume grad")
(let ((vol (make-instance 'volume :sx 2 :sy 3 :depth 4 :c 10.0)))
  (is (grad vol 1 1 2) 0.0 "Read grad")
  (is (cnn::setf-grad vol 1 1 2 15.0) 15.0 "set grad")
  (is (grad vol 1 1 2) 15.0 "Read grad 2")
  (setf (grad vol 1 1 2) 20.0)
  (is (grad vol 1 1 2) 20.0 "Read grad 3")
)

(diag "Volume add grad")
(let ((vol (make-instance 'volume :sx 2 :sy 3 :depth 4 :c 10.0)))
  (is (grad vol 1 1 2) 0.0 "Read grad")
  (add-grad vol 1 1 2 5.0)
  (is (grad vol 1 1 2) 5.0 "Read added grad")
  (add-grad vol 1 1 2 10.0)
  (is (grad vol 1 1 2) 15.0 "Read added grad")
)

(diag "Volume clone & 0")
(let ((vol (make-instance 'volume :sx 2 :sy 3 :depth 4 :c 10.0)))
  (let ((vol1 (clone-and-zero vol)))
    (ok vol1)
    (is (slot-value vol1 'cnn::sx) 2)
    (is (slot-value vol1 'cnn::sy) 3)
    (is (slot-value vol1 'cnn::depth) 4)
    
    (is (array-dimension (slot-value vol1 'cnn::w) 0) (* 2 3 4) "Volume allocation size")
    (is (array-dimension (slot-value vol1 'cnn::dw) 0) (* 2 3 4) "Volume allocation size")
    (is (value vol1 1 1 2) 0.0 "Read value")
    ))

(diag "Volume clone")
(let ((vol (make-instance 'volume :sx 2 :sy 3 :depth 4 :c 10.0)))
  (let ((vol1 (clone vol)))
    (ok vol1)
    (is (slot-value vol1 'cnn::sx) 2)
    (is (slot-value vol1 'cnn::sy) 3)
    (is (slot-value vol1 'cnn::depth) 4)
    
    (is (array-dimension (slot-value vol1 'cnn::w) 0) (* 2 3 4) "Volume allocation size")
    (is (array-dimension (slot-value vol1 'cnn::dw) 0) (* 2 3 4) "Volume allocation size")
    (is (value vol1 1 1 2) 10.0 "Read value")
    ))

(diag "Volume add")
(let ((vol (make-instance 'volume :sx 2 :sy 3 :depth 4 :c 10.0))
      (vol1 (make-instance 'volume :sx 2 :sy 3 :depth 4 :c 5.0)))
  (add vol vol1)
  (is (value vol 1 1 2) 15.0 "Read value")
  )

(diag "Volume add scaled")
(let ((vol (make-instance 'volume :sx 2 :sy 3 :depth 4 :c 10.0))
      (vol1 (make-instance 'volume :sx 2 :sy 3 :depth 4 :c 5.0)))
  (add-scaled vol vol1 10)
  (is (value vol 1 1 2) 60.0 "Read value")
  )

(diag "Volume add scaled")
(let ((vol (make-instance 'volume :sx 2 :sy 3 :depth 4 :c 10.0)))
  (setf (const vol) 20.0)
  (is (value vol 1 1 2) 20.0 "Read value")
  )


(finalize)
