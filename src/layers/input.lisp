(in-package #:cl-cnn)

(declaim (optimize (debug 3)))

(defclass input (layer)
  ((out-sx :initarg :sx :reader sx :documentation "Layer width")
   (out-sy :initarg :sy :reader sy :documentation "Layer height")
   (out-depth :initarg :depth :reader depth :documentation "Layer depth")
   )  
 (:documentation "Input Layer"))

(defmethod forward ((input input) (vol volume) &optional is-training)
  (setf (in-act input) vol)
  (setf (out-act input) vol)
  vol)

