(in-package #:cl-cnn)

(declaim (optimize (debug 3)))


(defclass volume ()
  ((sx :initarg :sx :documentation "Volume width")
   (sy :initarg :sy :documentation "Volume height")
   (depth :initarg :depth :documentation "Volume depth")
   (c :initarg :c)
   (w)
   (dw))
  (:documentation "VOLUME is the basic building block of all data in a NET.
  It is essentially just a 3D volume of numbers, with a
  width (SX), height (SY), and depth (DEPTH).
  it is used to hold data for all filters, all volumes,
  all weights, and also stores all gradients w.r.t. 
  the data. c is optionally a value to initialize the volume
  with. If c is missing, fills the Vol with random numbers."))


(defmethod initialize-instance :after ((object volume) &key)
;;  (declare (optimize (debug 3)))
  (with-slots (sx sy depth w dw) object
    (let ((n (* sx sy depth)))
	  (setf w (make-array n :initial-element 0.0))
	  (setf dw (make-array n :initial-element 0.0))
	  )))
