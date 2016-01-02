(in-package #:cl-cnn)

(declaim (optimize (debug 3)))

(defclass relu (layer)
  ((in-sx :initarg :sx :reader sx :documentation "Layer width")
   (in-sy :initarg :sy :reader sy :documentation "Layer height")
   (in-depth :initarg :depth :reader depth :documentation "Layer depth")
   (out-sx :accessor sx :documentation "Layer width")
   (out-sy :accessor sy :documentation "Layer height")
   (out-depth :accessor depth :documentation "Layer depth")
   )  
 (:documentation "Implements ReLU nonlinearity elementwise
$x -> max(0, x)$
the output is in [0, inf)"))

(defmethod initialize-instance :after ((object relu) &key)
;;  (declare (optimize (debug 3)))
  (with-slots (in-sx in-sy in-depth out-sx out-sy out-depth) object
    (setf out-sx in-sx)
    (setf out-sy in-sy)
    (setf out-depth in-depth)
    ))
(constructor relu)

(defmethod forward ((input relu) (vol volume) &optional is-training)
  (with-slots (in-act out-act out-depth filters biases) input
    (setf in-act vol)
    (let* ((v2 (clone col))
        (n (array-dimension (w vol) 0))
        (v2w (w v2)))
            (dotimes (i n)
                (when (< (aref v2w i) 0)
                    (setf (aref v2w i) 0)
                ))
            (setf out-act v2)
)))

(defmethod backward((input relu))
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
