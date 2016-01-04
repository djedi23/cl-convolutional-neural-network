(in-package #:cl-cnn)

(mgl-pax:defsection @main (:title "CL Convolutional Neural Networks")
  "## Installation

```cl
(ql:quickload :cl-cnn)
```

Translate from:
https://github.com/karpathy/convnetjs"
  (@volumes mgl-pax:section)
  (@net mgl-pax:section)

  (@layers mgl-pax:section)
  )

(mgl-pax:defsection @volumes (:title "Volumes")
"The entire library is based around transforming 3-dimensional volumes of numbers. These volumes are stored in the VOLUME class, which is at the heart of the library. The VOLUME class is a wrapper around:
 - a 1-dimensional list of numbers (the activations, in field .w)
 - their gradients (field .dw)
 - and lastly contains three dimensions (fields .sx, .sy, .depth).

Here are some examples:

```cl
;; create a VOLUME of size 32x32x3, and filled with random numbers
(volume :sx 32 :sy 32 :depth 3)
(volume :sx 32 :sy 32 :depth 3 :c 0.0) ; same volume but init with zeros
(volume :sx 1 :sy 1 :depth 3) ; a 1x1x3 VOLUME with random numbers
 
;; you can also initialize with a specific list. E.g. create a 1x1x3 Vol:
(volume :sx 1 :sy 1 :depth 3 :w #(1.2 3.5 3.6))
 
;; the VOLUME is a wrapper around two lists: .w and .dw, which both have 
;; sx * sy * depth number of elements. E.g:
(aref (w v) 0) ; contains 1.2
(aref (dw v) 0) ; contains 0, because gradients are initialized with zeros
 
;; you can also access the 3-D Vols with getters and setters
;; but these are subject to function call overhead
(let ((vol3d (volume :sx 10 :sy 10 :depth 5)))
  (setf (value vol3d 2 0 1) 5.0) ; set coordinate (2,0,1) to 5.0
  (value vol3d 2 0 1) ; returns 5.0
```
"

  (volume class)
  (volume function)
  (value (method () (volume t t t)))
  (setf-value (method () (volume t t t t)))
  (add-value (method () (volume t t t t)))
  (grad (method () (volume t t t)))
  (setf-grad (method () (volume t t t t)))
  (add-grad (method () (volume t t t t)))
  (clone (method () (volume)))
  (clone-and-zero (method () (volume)))
  (add (method () (volume volume)))
  (add-scaled (method () (volume volume t)))
  (set-const (method () (volume t)))
  )

(mgl-pax:defsection @net (:title "Networks")
  "A NET is a very simple class that simply contains a list of LAYER. When an example (in form of a VOLUME) is passed through the NET, the NET simply iterates through all of its layers and propagates the example through each one in turn, and returns the result of the last LAYER. Similarly, during backpropagation the Net calls the BACKWARD function of each layer in turn to compute the gradient."
  (net class)
  (add-layer (method () (net layer)))
  (forward (method () (net volume)))
  (backward (method () (net)))
  )

(mgl-pax:defsection @layers (:title "Layers")
"As mentioned, every Network (NET) is just a linear list of layers. Your first layer must be 'INPUT' (in which you declare sizes of your input), your last layer must be a LOSS layer ('SOFTMAX' or 'SVM' for classification, or 'REGRESSION' for regression). Every layer takes an input VOLUME and produces a new output VOLUME, which is why I prefer to refer to them as transformers.

Before going into details of the types of available layers, lets look at an example at this point that ties these concepts together in a concrete form:

```cl
;; minimal network: a simple binary SVM classifer in 2-dimensional space
(let ((net (make-instance 'net)))
  (add-layer net (input :out-sx 1 :out-sy 1 :out-depth 2))
  (add-layer net (svm))
  (add-layer net (fully-connected :num-classes 2))

  ;; create a 1x1x2 volume of input activations and
  ;; pass forward through network
  (let ((scores (forward net (volume :sx 1 :sy 1 :depth 2 :w #(0.5 -1.3)))))
    ;; scores is now a VOLUME of output activations
    (format t \"score for class 0 is assigned: ~a~%\"  (aref (cnn::w scores) 0))
    ))
```
 "

  (layer class)
  (initialize generic-function)
  (forward generic-function)
  (backward generic-function)
  (get-params-and-grads generic-function)
  
  (@input-layer mgl-pax:section)
  (@fully-connected-layer mgl-pax:section)
  (@loss-layers mgl-pax:section)
  )

(mgl-pax:defsection @input-layer (:title "Input Layers")
  "A dummy LAYER that essentially declares the size of input VOLUME and must be first layer in the network. Inputs other than real-valued numbers are currently not supported.
```cl
(input :out-sx 1 :out-sy 1 :out-depth 20) ;; declares 20-dimensional input points
(input :out-sx 24 :out-sy 24 :out-depth 3) ;; input is 24x24 RGB image
```
"
  (input class)
  (forward (method () (input volume)))
  )

(mgl-pax:defsection @fully-connected-layer (:title "Fully connected Layers")
"
Arguably the most important LAYER and building block of everything interesting. Declares a layer of neurons that perform weighted addition of all inputs (activations on layer below) and pass them through a nonlinearity. RELU is the best activation to use if you know nothing about these networks. However, you have to be careful with keeping learning rates small because ReLU units can permanently die if a large gradient pushes them off your data manifold. In pratice, you may want to chain a few of these depending on how deep you want your deep learning to be ;) A good rule of thumb is you want just a few - maybe 1-3, unless you have really large datasets.

```cl
;; create layer of 10 linear neurons (no activation function by default)
(fully-connected :num-neurons 10)
;; create layer of 10 neurons that use sigmoid activation function
(fully-connected :num-neurons 10 :activation 'sigmoid) ;; x->1/(1+e^(-x))
(fully-connected :num-neurons 10 :activation 'tanh) ;; x->tanh(x)
(fully-connected :num-neurons 10 :activation 'relu) ;; rectified linear units: x->max(0,x)
;; maxout units: (x,y)->max(x,y). num-neurons must be divisible by 2.
;; maxout \"consumes\" multiple filters for every output. Thus, this line
;; will actually produce only 5 outputs in this layer. (group-size is 2)
;; by default.
(fully-connected :num-neurons 10 :activation 'maxout) 
;; specify group size in maxout. num-neurons must be divisible by group-size.
;; here, output will be 3 neurons only (3 = 12/4)
(fully-connected :num-neurons 12, group-size: 4 :activation 'maxout)
;; dropout half the units (probability 0.5) in this layer during training, for regularization
(fully-connected :num-neurons 10 :activation 'relu', drop-prob: 0.5)
```
"


  (fully-connected function)
  (fully-connected class)
  (forward (method () (fully-connected volume)))
  (backward (method () (fully-connected)))
  )

(mgl-pax:defsection @loss-layers (:title "Loss Layers")
"
Use these if you are interested in predicting a set of discrete classes for your data. In SOFTMAX, the outputs are probabilities that sum to 1. An SVM is trained to only output scores, not probabilities. SVMs also use a bit better loss function that is more robust (a hinge loss), but its best to experiment a bit.

```cl
(softmax :num-classes 2)
(svm :num-classes 2)
```
When you are training a classifier LAYER, your classes must be numbers that begin at 0. For a binary problem, these would be class 0 and class 1. For K classes, the classes are 0..K-1.

Layers that implement a loss. Currently these are the layers that 
 can initiate a BACKWARD pass. In future we probably want a more 
 flexible system that can accomodate multiple losses to do multi-task
 learning, and stuff like that. But for now, one of the layers in this
 file must be the final LAYER in a NET."

  (@softmax-layer mgl-pax:section)
  (@svm-layer mgl-pax:section)
)

(mgl-pax:defsection @softmax-layer (:title "Softmax Layers")
"This is a classifier, with N discrete classes from 0 to N-1
it gets a stream of N incoming numbers and computes the softmax
function (exponentiate and normalize to sum to 1 as probabilities should)"
  (softmax class)
  (forward (method () (softmax volume)))
  (backward (method () (softmax)))
)

(mgl-pax:defsection @svm-layer (:title "SVM Layers")
""
  (svm class)
  (forward (method () (svm volume)))
  (backward (method () (svm)))
)


(defun make-readme.md ()
  (with-open-file (s (asdf:system-relative-pathname :cl-cnn "README.md")
		     :direction :output :if-exists :supersede)
    (mgl-pax:document @main :stream s))
  (update-wiki))


(defun update-wiki ()
  (let ((sections (list
		   @softmax-layer
		   @svm-layer
		   @loss-layers
		   @fully-connected-layer
		   @input-layer
		   @layers
		   @net
		   @volumes
		   @main
		   )))
    (mgl-pax:document
     ;; This could be a list of objects (typically sections), if you
     ;; want to generate documentation for related libraries with
     ;; cross-links.
     @main
     :pages (loop for section in sections
	       collect (section-to-page-spec section))
     )))

(defun section-to-filename (section)
  (asdf:system-relative-pathname
   :cl-cnn
   (format nil "../cl-cnn.wiki/~a.md"
           (string-downcase (subseq (symbol-name (mgl-pax:section-name section)) 1)))))

(defun section-to-page-spec (section &key
				       (open-args '(:if-does-not-exist :create
						    :if-exists :supersede
						    :ensure-directories-exist t)))
  `(:objects
    (,section)
    :output (,(section-to-filename section) ,@open-args)
    :uri-fragment ,(format nil "https://git.valvassori.info/djedi/cl-cnn/wikis/~a" (string-downcase (subseq (symbol-name (mgl-pax:section-name section)) 1)))
    ))
