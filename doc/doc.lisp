(in-package #:cl-cnn)

(mgl-pax:defsection @main (:title "CL Convolutional Neural Networks")
  "## Installation

```lisp
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

```js
// create a Vol of size 32x32x3, and filled with random numbers
var v = new convnetjs.Vol(32, 32, 3);
var v = new convnetjs.Vol(32, 32, 3, 0.0); // same volume but init with zeros
var v = new convnetjs.Vol(1, 1, 3); // a 1x1x3 Vol with random numbers
 
// you can also initialize with a specific list. E.g. create a 1x1x3 Vol:
var v = new convnetjs.Vol([1.2, 3.5, 3.6]);
 
// the Vol is a wrapper around two lists: .w and .dw, which both have 
// sx * sy * depth number of elements. E.g:
v.w[0] // contains 1.2
v.dw[0] // contains 0, because gradients are initialized with zeros
 
// you can also access the 3-D Vols with getters and setters
// but these are subject to function call overhead
var vol3d = new convnetjs.Vol(10, 10, 5);
vol3d.set(2,0,1,5.0); // set coordinate (2,0,1) to 5.0
vol3d.get(2,0,1) // returns 5.0
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
  )

(mgl-pax:defsection @layers (:title "Layers")
"As mentioned, every Network (NET) is just a linear list of layers. Your first layer must be 'INPUT' (in which you declare sizes of your input), your last layer must be a LOSS layer ('SOFTMAX' or 'SVM' for classification, or 'REGRESSION' for regression). Every layer takes an input VOLUME and produces a new output VOLUME, which is why I prefer to refer to them as transformers.

Before going into details of the types of available layers, lets look at an example at this point that ties these concepts together in a concrete form:

```javascript
var layer_defs = [];
// minimal network: a simple binary SVM classifer in 2-dimensional space
layer_defs.push({type:'input', out_sx:1, out_sy:1, out_depth:2});
layer_defs.push({type:'svm', num_classes:2});
 
// create a net
var net = new convnetjs.Net();
net.makeLayers(layer_defs);
 
// create a 1x1x2 volume of input activations:
var x = new convnetjs.Vol(1,1,2);
x.w[0] = 0.5; // w is the field holding the actual data
x.w[1] = -1.3;
// a shortcut for the above is var x = new convnetjs.Vol([0.5, -1.3]);
 
var scores = net.forward(x); // pass forward through network
// scores is now a Vol() of output activations
console.log('score for class 0 is assigned:'  + scores.w[0]); 
```
 "

  (layer class)
  (forward generic-function)
  (backward generic-function)
  (get-params-and-grads generic-function)
  
  (@input-layer mgl-pax:section)
  (@fully-connected-layer mgl-pax:section)
  )

(mgl-pax:defsection @input-layer (:title "Input Layers")
  (input class)
  (forward (method () (input volume)))
  )

(mgl-pax:defsection @fully-connected-layer (:title "Fully connected Layers")
  (fully-connected class)
  (forward (method () (fully-connected volume)))
  (backward (method () (fully-connected)))
  )



(defun make-readme.md ()
  (with-open-file (s (asdf:system-relative-pathname :cl-cnn "README.md")
		     :direction :output :if-exists :supersede)
    (mgl-pax:document @main :stream s))
  (update-wiki))


(defun update-wiki ()
  (let ((sections (list
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
