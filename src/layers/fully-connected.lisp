(in-package #:cl-cnn)

(declaim (optimize (debug 3)))

(defclass fully-connected (layer)
  ((l1-decay-mul :initarg :l1-decay-mul :reader l1-decay-mul :initform 0.0)
   (l2-decay-mul :initarg :l2-decay-mul :reader l2-decay-mul :initform 1.0)

   (num-inputs :reader num-inputs)
   filters
   (bias :initarg :bias :initform 0.0 :documentation "Initial bias")
   biases
   )  
  (:documentation "Full connected Layer"))

(constructor fully-connected)
(defmethod initialize ((object fully-connected))
  ;;  (declare (optimize (debug 3)))
  (with-slots (in-sx in-sy in-depth out-depth num-inputs filters bias biases) object
    (setf num-inputs (* in-sx in-sy in-depth))
    (setf filters (make-array out-depth))
    (loop for filter across filters
       for i from 0
       do
	 (setf (aref filters i) (volume :sx 1 :sy 1 :depth num-inputs)))
    (setf biases (volume :sx 1 :sy 1 :depth out-depth :c bias))
    ))

(defmethod forward ((input fully-connected) (vol volume) &optional is-training)
  (with-slots (in-act out-act out-depth filters biases) input
    (setf in-act vol)
    (let ((A (volume :sx 1 :sy 1 :depth out-depth :c 0.0))
	  (Vw (w vol)))
      (dotimes (i out-depth)
	(let ((aa 0.0)
	      (wi (w (aref filters i))))
	  (dotimes (d (num-inputs input))
	    (incf aa #i(Vw[d] * wi[d])))
	  (incf aa (aref (w biases) i))
	  (setf (value A 0 0 i) aa)
	  ))
      (setf out-act A)
      out-act
      )))

(defmethod backward ((input fully-connected) &optional index)
  (with-slots (in-act out-act out-depth filters biases) input
    (let* ((V in-act))
      (setf (dw V) (make-array (array-dimension (w V) 0) :initial-element 0.0))
      (dotimes (i out-depth)
	(let ((tfi (aref filters i))
	      (chain_grad (aref (dw out_act) i)))
	  (dotimes (d (num-inputs input))
	    (incf (aref (dw V) d) (* (aref (w tfi) d) chain_grad))
	    (incf (aref (dw tfi) d) (* (aref (w V) d) chain_grad)))
	  (incf (aref (dw biases) i) chain_grad)
	  )))))
