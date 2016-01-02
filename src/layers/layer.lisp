(in-package #:cl-cnn)

(declaim (optimize (debug 3)))

(defclass layer ()
  ((in-act :accessor in-act :documentation "input activation vector")
   (out-act :accessor out-act :documentation "output activation vector")
   ))


(defgeneric forward (layer-or-net volume &optional is-training)
  (:documentation "Forward the values of VOLUME in the LAYER or the NET"))

(defgeneric backward (layer-or-net &optional index)
  (:documentation "Backpropagation"))

(defgeneric get-params-and-grads (layer-or-net)
  (:documentation ""))
