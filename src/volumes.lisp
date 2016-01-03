(in-package #:cl-cnn)

(declaim (optimize (debug 3)))


(defclass volume ()
  ((sx :initarg :sx :reader sx :documentation "Volume width")
   (sy :initarg :sy :reader sy :documentation "Volume height")
   (depth :initarg :depth :reader depth :documentation "Volume depth")
   (c :initarg :c)
   (w :initarg :w :reader w)
   (dw :accessor dw))
  (:documentation "VOLUME is the basic building block of all data in a NET.
  It is essentially just a 3D volume of numbers, with a
  width (SX), height (SY), and depth (DEPTH).
  it is used to hold data for all filters, all volumes,
  all weights, and also stores all gradients w.r.t. 
  the data. C is optionally a value to initialize the VOLUME
  with. If C is missing, fills the VOLUME with random numbers."))

(constructor volume object
  ;;  (declare (optimize (debug 3)))
  (with-slots (sx sy depth c w dw) object
    (let ((n (* sx sy depth)))
      (if (slot-boundp object 'w)
	  (setf w (adjust-array w n))
	  (progn
	    (setf w (make-array n :initial-element (if (slot-boundp object 'c) c 0)))
	    (unless (slot-boundp object 'c)
	      (dotimes (i n)
		(setf (aref w i) (random (sqrt (/ 1 n)))))
	      )))
      (setf dw (make-array n :initial-element 0.0))
      )))
(defmethod print-object ((o volume) stream)
  (format stream "#<VOLUME ~a>" (w o)))


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

(defmethod clone-and-zero ((vol volume))
  (make-instance 'volume :sx (sx vol) :sy (sy vol) :depth (depth vol) :c 0.0))

(defmethod clone ((vol volume))
  (let ((new (clone-and-zero vol)))
    (setf (slot-value new 'w) (make-array (array-dimension (slot-value vol 'w) 0)
					  :initial-contents (slot-value vol 'w)))
    new
    ))

(defmethod add ((v1 volume) (v2 volume))
  "Add value of V2 in V1"
  (loop for wv1 across (slot-value v1 'w)
     for wv2 across (slot-value v2 'w)
     for i from 0 
     do
       (setf (aref (slot-value v1 'w) i)
	     (+ wv1 wv2)))
  v1)

(defmethod add-scaled ((v1 volume) (v2 volume) scale)
  "Add SCALED value of V2 in V1"
  (loop for wv1 across (slot-value v1 'w)
     for wv2 across (slot-value v2 'w)
     for i from 0 
     do
       (setf (aref (slot-value v1 'w) i)
	     (+ wv1 (* scale wv2))))
  v1)

(defmethod set-const ((vol volume) v)
  "Set a constant Value in VOL"
  (setf (slot-value vol 'w) (make-array (array-dimension (slot-value vol 'w) 0)
					:initial-element v))
  vol
  )
(defsetf const set-const "Set constant")
