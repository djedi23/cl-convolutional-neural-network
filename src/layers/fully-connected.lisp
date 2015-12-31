(in-package #:cl-cnn)

(declaim (optimize (debug 3)))

(defclass fully-connected (layer)
  (
   (in-sx :initarg :sx :reader sx :documentation "Layer width")
   (in-sy :initarg :sy :reader sy :documentation "Layer height")
   (in-depth :initarg :depth :reader depth :documentation "Layer depth")
   (out-depth :initarg :num-neurons :documentation "Number of neurons")
   (out-sx :initform 1 :documentation "Layer width")
   (out-sy :initform 1 :documentation "Layer height")

   (l1-decay-mul :initarg :l1-decay-mul :reader l1-decay-mul :initform 0.0)
   (l2-decay-mul :initarg :l2-decay-mul :reader l2-decay-mul :initform 1.0)

   (num-inputs :reader num-inputs)
   filters
   (bias :initarg :bias :initform 0.0 :documentation "Initial bias")
   biases
   )  
 (:documentation "Full connected Layer"))

(defmethod initialize-instance :after ((object fully-connected) &key)
;;  (declare (optimize (debug 3)))
  (with-slots (in-sx in-sy in-depth out-depth num-inputs filters bias biases) object
    (setf num-inputs (* in-sx in-sy in-depth))
    (setf filters (make-array out-depth))
    (loop for filter across filters
       for i from 0
       do
	 (setf (aref filters i) (make-instance 'volume :sx 1 :sy 1 :depth num-inputs)))
    (setf biases (make-instance 'volume :sx 1 :sy 1 :depth out-depth :c bias))
    ))

(defmethod forward ((input fully-connected) (vol volume) &optional is-training)
  (setf (in-act input) vol)
  (setf (out-act input) vol)
  vol)

