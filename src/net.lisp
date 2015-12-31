(in-package #:cl-cnn)

(declaim (optimize (debug 3)))

(defclass net ()
  ((layers :initform '()))
  (:documentation "Net manages a set of layers

For now constraints: Simple linear order of layers, first layer input last layer a cost layer"))


(defmethod make-layers ((net net) defs)

  )
