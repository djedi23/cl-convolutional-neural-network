(in-package #:cl-cnn)

(declaim (optimize (debug 3)))

(defclass net ()
  ((layers :initform (make-array 2 :adjustable t :fill-pointer 0) :accessor layers))
  (:documentation "Net manages a set of layers

For now constraints: Simple linear order of layers, first layer input last layer a cost layer"))
(constructor net)

(defmethod make-layers ((net net) defs)

  )

(defmethod add-layer ((net net) (layer layer))
  "Push a LAYER in the end of the NET "
  (when (> (fill-pointer (layers net)) 0)
    (let ((prev (aref (layers net) (- (fill-pointer (layers net)) 1))))
      (setf (in-sx layer) (out-sx prev))
      (setf (in-sy layer) (out-sy prev))
      (setf (in-depth layer) (out-depth prev))
      ))
  (initialize layer)
  (vector-push-extend layer (layers net))
  net)
