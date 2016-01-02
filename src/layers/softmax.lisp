(in-package #:cl-cnn)

(declaim (optimize (debug 3)))

(defclass softmax (layer)
  ((num-inputs :reader num-inputs)
   (es :accessor es))  
  (:documentation "Implements Softmax loss"))
(constructor softmax)

(defmethod initialize ((object softmax))
  ;;  (declare (optimize (debug 3)))
  (with-slots (in-sx in-sy in-depth out-sx out-sy out-depth num-inputs) object
    (setf num-inputs (* in-sx in-sy in-depth))
    (setf out-sx 1)
    (setf out-sy 1)
    (setf out-depth num-inputs)
    ))

(defmethod forward ((input softmax) (vol volume) &optional is-training)
  (with-slots (in-act out-act out-depth) input
    (setf in-act vol)
    (let* ((a (volume :sx 1 :sy 1 :depth out-depth :c 0.0))
	   (as (w vol))
	   (amax (aref (w vol) 0))
	   (es (make-array out-depth :initial-element 0.0))
	   (esum 0.0))
      (dotimes (i out-depth)
	(when (> (aref as i) amax)
	  (setf amax (aref as i))))
      (dotimes (i out-depth)
	(let ((e (exp #i(as[i] - amax))))
	  (incf esum e)
	  (setf (aref es i) e)))
      (dotimes (i out-depth)
	(setf (aref es i) (/ (aref es i) esum))
	(setf (aref (w a) i) (aref es i)))

      (setf (es input) es)
      (setf out-act a)
      )))

(defmethod backward((input softmax) &optional y)
  (with-slots (in-act out-act out-depth) input
    (let* ((x in-act))
      (setf (dw x) (make-array (array-dimension (w x) 0) :initial-element 0.0))
      (dotimes (i out-depth)
	(let ((indicator (if (= i y) 1.0 .0.0)))
	  (setf (aref (dw x) i) (- (- indicator (aref (es indicator) i))))
	  ))
      (- (log (aref (es indicator) y)))
      )))

