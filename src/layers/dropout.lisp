(in-package #:cl-cnn)

(declaim (optimize (debug 3)))


(defclass dropout (layer)
  ((drop-prob :initarg :drop-prob :initform 0.5)
   (dropped))

  (:documentation "An inefficient dropout layer

  Note this is not most efficient implementation since the layer before
  computed all these activations and now we're just going to drop them :(
  same goes for backward pass. Also, if we wanted to be efficient at test time
  we could equivalently be clever and upscale during train and copy pointers during test
  todo: make more efficient.
"))

(constructor dropout)
(defmethod initialize ((object dropout))
  (with-slots (in-sx in-sy in-depth out-sx out-sy out-depth dropped) object
    (setf out-sx in-sx)
    (setf out-sy in-sy)
    (setf out-depth in-depth)
    (setf dropped (make-array (* out-sx out-sy out-depth) :initial-element 0.0))))

(defmethod forward ((input dropout) (vol volume) &optional is-training)
  (with-slots (in-act out-act drop-prob) input
    (setf in-act vol)
    (let ((v2 (clone vol))
	  (n (array-dimension (w vol) 0)))
      (if is-training
	  (dotimes (i n)
	    (if (< (random 1.0) drop-prob)
		(progn
		  (setf (aref (w v2) i) 0.0)
		  (setf (aref dropped i) t))
		(setf (aref dropped i) nil)
		))
	  (dotimes (i n)
	    (if (< (random 1.0) drop-prob)
		(setf (aref (w v2) i) (* (aref (w v2) i) drop-prob))
		)))
      (setf out-act v2)
      v2
      )))


(defmethod backward((input dropout) &optional y)
  (with-slots (in-act out-act out-depth dropped) input
    (let ((v in-act)
	  (chain-grad out-act)
	  (n (array-dimension (w in-act) 0)))
      (setf (dw v) (make-array n :initial-element 0.0))
      (dotimes (i n)
	(unless (aref dropped i)
	  (setf (aref (dw v) i) (aref (dw chain-grad) i))
      )))))
