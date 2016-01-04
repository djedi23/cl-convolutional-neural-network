(in-package #:cl-cnn)

(declaim (optimize (debug 3)))


(defclass sigmoid (activations layer)
  ())
(constructor sigmoid)
(defmethod initialize ((object sigmoid))
  (with-slots (in-sx in-sy in-depth out-sx out-sy out-depth) object
    (setf out-sx in-sx)
    (setf out-sy in-sy)
    (setf out-depth in-depth)))

(defmethod forward ((input sigmoid) (vol volume) &optional is-training)
  (with-slots (in-act out-act ) input
    (setf in-act vol)
    (let* ((v2 (clone-and-zero vol))
	   (n (array-dimension (w vol) 0))
	   (v2w (w v2))
	   (vw (w vol)))
      (dotimes (i n)
	(setf (aref v2w i) #i(1.0 / (1.0 + exp(-vw[i])))))
      (setf out-act v2)
      v2
      )))


(defmethod backward((input sigmoid) &optional y)
  (with-slots (in-act out-act out-depth) input
    (let ((v in-act)
	  (v2 out-act)
	  (n (array-dimension (w in-act) 0)))
      (setf (dw v) (make-array n :initial-element 0.0))
      (dotimes (i n)
	(let ((v2wi (aref (w v2) i)))
	  (setf (aref (dw v) i) #i(v2wi * (1.0 - v2wi) * dw(v2)[i]) )
	  )))))
