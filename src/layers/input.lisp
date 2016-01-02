(in-package #:cl-cnn)

(declaim (optimize (debug 3)))

(defclass input (layer)
  ()  
 (:documentation "Input Layer"))
(constructor input)

(defmethod forward ((input input) (vol volume) &optional is-training)
  (setf (in-act input) vol)
  (setf (out-act input) vol)
  vol)

