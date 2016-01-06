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
	      ;; -- start convolution --
	      (let ((aa 0.0))
		(dotimes (fy (sy f))
		  (let ((oy (+ y fy)))
		    (dotimes (fx (sx f))
		      (let ((ox (+ x fx)))
			(when (and (>= oy 0) (< oy vsy) (>= ox 0) (< ox vsx))
			  (dotimes (fd (depth f))
			    (incf aa #i(w(f[((sx(f)*fy)+fx)*depth(f)+fd] *
					    w(vol[((vsx * oy)+ox)*depth(vol)+fd]))))
			    ))))))
		(incf aa (aref (w biases) d))
		(setf (value a ax ay d) aa))
	      ;; -- end convolution --
	      ))))
      (setf out-act A)
      out-act
      )))


(defmethod backward ((input convolutional) &optional index)
  (with-slots (in-act out-act out-depth pad filters biases) input
    (let* ((V in-act)
	   (vsx (sx vol))
	   (vsy (sy vol))
	   (xystride stride))
      (setf (dw V) (make-array (array-dimension (w V) 0) :initial-element 0.0))
      (loop for d from 0 below out-depth
	 for f = (aref filters d)
	 for x = (- pad)
	 for y = (- pad)
	 do
	   (loop for ay from 0 below out-sy
	      for y = (+ y xystride)
	      for x = (- pad)
	      do
		(loop for ax from 0 below out-sx
		   for x = (+ x xystride)
		   for chain-grad = (get-grad out-act ax ay d)
		   do
		   ;; ---
		     (loop for fy from 0 below (sy f)
			for oy = (+ y fy)
			do
			  (loop for fx from 0 below (sx f)
			     for ox = (+ x fx)
			     when (and (>= oy 0) (< oy vsy) (>= ox 0) (< ox vsx))
			     do
			       (loop for fd from 0 below (depth f)
				  for ix1 = #i(((vsx * oy)+ox)* depth(v)+fd)
				  for ix2 = #i(((sx(f) * fy)+fx)*depth(f)+fd)
				  do
				    (incf #i(dw(f)[ix2]) #i(w(v)[ix1]*!chain-grad))
				    (incf #i(dw(v)[ix1]) #i(w(f)[ix2]*!chain-grad))
				    ))) ;; ---
		     (incf (aref (dw biases) d) chain-grad)
		     )))
      )))
