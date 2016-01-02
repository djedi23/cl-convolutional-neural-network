(in-package #:cl-cnn)

(declaim (optimize (debug 3)))

(defclass relu (layer)
  ()  
  (:documentation "Implements ReLU nonlinearity elementwise
$x -> max(0, x)$
the output is in [0, inf)"))
(constructor relu)

(defmethod initialize ((object relu))
  ;;  (declare (optimize (debug 3)))
  (with-slots (in-sx in-sy in-depth out-sx out-sy out-depth) object
    (setf out-sx in-sx)
    (setf out-sy in-sy)
    (setf out-depth in-depth)
    ))

(defmethod forward ((input relu) (vol volume) &optional is-training)
  (with-slots (in-act out-act out-depth filters biases) input
    (setf in-act vol)
    (let* ((v2 (clone vol))
	   (n (array-dimension (w vol) 0))
	   (v2w (w v2)))
      (dotimes (i n)
	(when (< (aref v2w i) 0)
	  (setf (aref v2w i) 0)
	  ))
      (setf out-act v2)
      )))

(defmethod backward((input relu) &optional index)
  (with-slots (in-act out-act out-depth filters biases) input
    ;;    (setf in-act vol)
    (let* ((v in-act)
	   (v2 out-act)
	   (n (array-dimension (w v) 0))
	   )
      (setf (dw v) (make-array (array-dimension n 0) :initial-element 0.0))
      (dotimes (i n)
	(if (<= (aref (w v2) i) 0)
	    (setf (aref (dw v) i) 0)
	    (setf (aref (dw v) i) (aref (dw v2) i))
	    ))
      out-act
      )))
