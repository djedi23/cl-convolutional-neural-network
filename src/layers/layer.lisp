(in-package #:cl-cnn)

(declaim (optimize (debug 3)))

(defclass layer ()
  ((in-act :accessor in-act :documentation "input activation vector")
   (out-act :accessor out-act :documentation "output activation vector")

   (in-sx :initarg :in-sx :accessor in-sx :documentation "Layer width")
   (in-sy :initarg :in-sy :accessor in-sy :documentation "Layer height")
   (in-depth :initarg :in-depth :accessor in-depth :documentation "Layer depth")
   (out-sx :initarg :out-sx :initform 1 :accessor out-sx :documentation "Layer width")
   (out-sy :initarg :out-sy :initform 1 :accessor out-sy :documentation "Layer height")
   (out-depth :initarg :out-depth :initarg :num-neurons :accessor out-depth :documentation "Number of neurons")
   ))

(defgeneric initialize (layer)
  (:documentation "post initialize the layer"))
(defmethod initialize ((layer layer)))


(defgeneric forward (layer-or-net volume &optional is-training)
  (:documentation "Forward the values of VOLUME in the LAYER or the NET"))

(defgeneric backward (layer-or-net &optional index)
  (:documentation "Backpropagation"))

(defgeneric get-params-and-grads (layer-or-net)
  (:documentation ""))
