(in-package #:cl-cnn)

(declaim (optimize (debug 3)))

(defclass svm (loss layer)
  ((num-inputs :reader num-inputs))
  )
(constructor svm)
(defmethod initialize ((object svm))
  (with-slots (in-sx in-sy in-depth out-sx out-sy out-depth num-inputs) object
    (setf num-inputs (* in-sx in-sy in-depth))
    (setf out-sx 1)
    (setf out-sy 1)
    (setf out-depth num-inputs)
    ))

(defmethod forward ((input svm) (vol volume) &optional is-training)
  (with-slots (in-act out-act) input
    (setf in-act vol)
    (setf out-act vol)
    vol))

(defmethod backward((input svm) &optional y)
  (with-slots (in-act out-act out-depth) input
    (let* ((x in-act)
	   (yscore (aref (w x) y))
	   (margin 1.0)
	   (loss 0.0))
      (setf (dw x) (make-array (array-dimension (w x) 0) :initial-element 0.0))
      (dotimes (i out-depth)
	(unless (= y i)
	  (let ((ydiff (+ (aref (w x) i) (- yscore) margin )))
	    (when (> ydiff 0.0)
	      (incf (aref (dw x) i))
	      (decf (aref (dw x) y))
	      (incf loss ydiff)
	      ))))
      loss
      )))
