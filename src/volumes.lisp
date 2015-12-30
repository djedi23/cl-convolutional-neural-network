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
  the data. C is optionally a value to initialize the VOLUME
  with. If C is missing, fills the VOLUME with random numbers."))


(defmethod initialize-instance :after ((object volume) &key)
;;  (declare (optimize (debug 3)))
  (with-slots (sx sy depth c w dw) object
    (let ((n (* sx sy depth)))
	  (setf w (make-array n :initial-element (if (slot-boundp object 'c) c 0)))
	  (setf dw (make-array n :initial-element 0.0))
    (unless (slot-boundp object 'c)
  	  (dotimes (i n)
	      (setf (aref w i) (random (sqrt (/ 1 n)))))
	  ))))


(defun index-of (volume x y d)
  (with-slots (sx depth) volume
    #i((sx * y)+x)* depth + d))


(defmethod value ((vol volume) x y d)
  "Read the value at X Y D of the VOLUME."
  (aref (slot-value vol 'w) (index-of vol x y d)))

(defmethod setf-value ((vol volume) x y d v)
  "Setter for VALUE"
  (setf (aref (slot-value vol 'w) (index-of vol x y d)) v))

(defsetf value setf-value "Setter for value")

(defmethod add-value ((vol volume) x y d v)
  "Adder for VALUE"
  (incf (aref (slot-value vol 'w) (index-of vol x y d)) v))



(defmethod grad ((vol volume) x y d)
  "Read the grad at X Y D of the VOLUME."
  (aref (slot-value vol 'dw) (index-of vol x y d)))

(defmethod setf-grad ((vol volume) x y d v)
  "Setter for GRAD"
  (setf (aref (slot-value vol 'dw) (index-of vol x y d)) v))

(defsetf grad setf-grad "Setter for grad")

(defmethod add-grad ((vol volume) x y d v)
  "Adder for GRAD"
  (incf (aref (slot-value vol 'dw) (index-of vol x y d)) v))


