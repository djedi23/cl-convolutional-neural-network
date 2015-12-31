(in-package #:cl-cnn)

(declaim (optimize (debug 3)))

(defclass layer ()
  ((in-act :accessor in-act :documentation "input activation vector")
   (out-act :accessor out-act :documentation "output activation vector")
   ))


(defgeneric forward (volume &optional is-training)
  (:documentation "Forward the values of VOLUME"))
