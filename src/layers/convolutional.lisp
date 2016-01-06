(in-package #:cl-cnn)

(declaim (optimize (debug 3)))


(defclass convolutional (layer)
  ((sx :initarg :sx :reader sx :documentation "filter size. Should be odd if possible, it's cleaner.")
   (sy :initarg :sy :reader sy)
   (stride :initarg :stride :reader stride :initform 1 :documentation "stride at which we apply filters to input volume")
   (pad :initarg :pad :reader pad :initform 0)
   (l1-decay-mul :initarg :l1-decay-mul :reader l1-decay-mul :initform 0.0)
   (l2-decay-mul :initarg :l2-decay-mul :reader l2-decay-mul :initform 1.0)
   (num-inputs :reader num-inputs)
   filters
   (bias :initarg :bias :initform 0.0 :documentation "Initial bias")
   biases
   )
  (:documentation "does convolutions (so weight sharing spatially) putting them together in one file because they are very similar"))

(constructor convolutional)

(defmethod initialize ((object convolutional))
  ;;  (declare (optimize (debug 3)))
  (with-slots (sx sy in-sx in-sy out-sx out-sy in-depth out-depth pad stride num-inputs filters bias biases) object
    (unless (slot-boundp object 'sy)
      (setf sy sx))
    (setf out-sx (floor #i( (!in-sx + pad * 2 - sx ) / stride + 1 )))
    (setf out-sy (floor #i( (!in-sy + pad * 2 - sy ) / stride + 1 )))
    (setf num-inputs (* in-sx in-sy in-depth))
    (setf filters (make-array out-depth))
    (loop for filter across filters
       for i from 0
       do
	 (setf (aref filters i) (volume :sx 1 :sy 1 :depth in-depth)))
    (setf biases (volume :sx 1 :sy 1 :depth out-depth :c bias))
    ))


(defmethod forward ((input convolutional) (vol volume) &optional is-training)
  (with-slots (in-act out-act out-sx out-sy out-depth stride pad filters biases) input
    (setf in-act vol)
    (let ((A (volume :sx out-sx :sy out-sy :depth out-depth :c 0.0))
	  (vsx (sx vol))
	  (vsy (sy vol))
	  (xystride stride))
      (dotimes (d out-depth)
	(let ((f (aref filters d))
	      (x (- pad))
	      (y (- pad)))
	  (dotimes (ay out-sy)
	    (incf y xystride)
	    (setf x (- pad))
	    (dotimes (ax out-sx)
	      (incf x xystride)

	      (let ((aa 0.0))
		(dotimes (fy (sy f))
		  (let ((oy (+ y fy)))
		    (dotimes (fx (sx f))
		      (let ((ox (+ x fx)))
			(when (and (>= oy 0) (< oy vsy) (>= ox 0) (< ox vsx))
			  (dotimes (fd (depth f))
			    (incf a #i(3)) ;; -----
			    )))))))

	      ))))
      (setf out-act A)
      out-act
      )))

#+todo
(defmethod backward ((input convolutional) &optional index)
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
