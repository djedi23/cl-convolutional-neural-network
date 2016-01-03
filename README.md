<a id='x-28CL-CNN-3A-40MAIN-20MGL-PAX-3ASECTION-29'></a>

# CL Convolutional Neural Networks

## Table of Contents

- [1 Volumes][458c]
- [2 Networks][3b14]
- [3 Layers][85dc]
    - [3.1 Input Layers][42a3]
    - [3.2 Fully connected Layers][29e6]
    - [3.3 Loss Layers][a784]
        - [3.3.1 Softmax Layers][339e]
        - [3.3.2 SVM Layers][aeeb]

###### \[in package CL-CNN\]
## Installation

```cl
(ql:quickload :cl-cnn)
```

Translate from:
https://github.com/karpathy/convnetjs

<a id='x-28CL-CNN-3A-40VOLUMES-20MGL-PAX-3ASECTION-29'></a>

## 1 Volumes

The entire library is based around transforming 3-dimensional volumes of numbers. These volumes are stored in the [`VOLUME`][dfd4] class, which is at the heart of the library. The [`VOLUME`][dfd4] class is a wrapper around:
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


<a id='x-28CL-CNN-3AVOLUME-20CLASS-29'></a>

- [class] **VOLUME**

    `VOLUME` is the basic building block of all data in a [`NET`][2a65].
    It is essentially just a 3D volume of numbers, with a
    width (SX), height (SY), and depth (`DEPTH`).
    it is used to hold data for all filters, all volumes,
    all weights, and also stores all gradients w.r.t. 
    the data. C is optionally a value to initialize the `VOLUME`
    with. If C is missing, fills the `VOLUME` with random numbers.

<a id='x-28CL-CNN-3AVOLUME-20FUNCTION-29'></a>

- [function] **VOLUME** *&REST REST*

    Constructor for the class [`VOLUME`][dfd4]

<a id='x-28CL-CNN-3AVALUE-20-28METHOD-20NIL-20-28CL-CNN-3AVOLUME-20T-20T-20T-29-29-29'></a>

- [method] **VALUE** *(VOL VOLUME) X Y D*

    Read the value at `X` `Y` `D` of the `VOLUME`([`0`][d757] [`1`][dfd4]).

<a id='x-28CL-CNN-3A-3ASETF-VALUE-20-28METHOD-20NIL-20-28CL-CNN-3AVOLUME-20T-20T-20T-20T-29-29-29'></a>

- [method] **SETF-VALUE** *(VOL VOLUME) X Y D V*

    Setter for [`VALUE`][1d3d]

<a id='x-28CL-CNN-3AADD-VALUE-20-28METHOD-20NIL-20-28CL-CNN-3AVOLUME-20T-20T-20T-20T-29-29-29'></a>

- [method] **ADD-VALUE** *(VOL VOLUME) X Y D V*

    Adder for [`VALUE`][1d3d]

<a id='x-28CL-CNN-3AGRAD-20-28METHOD-20NIL-20-28CL-CNN-3AVOLUME-20T-20T-20T-29-29-29'></a>

- [method] **GRAD** *(VOL VOLUME) X Y D*

    Read the grad at `X` `Y` `D` of the `VOLUME`([`0`][d757] [`1`][dfd4]).

<a id='x-28CL-CNN-3A-3ASETF-GRAD-20-28METHOD-20NIL-20-28CL-CNN-3AVOLUME-20T-20T-20T-20T-29-29-29'></a>

- [method] **SETF-GRAD** *(VOL VOLUME) X Y D V*

    Setter for [`GRAD`][674e]

<a id='x-28CL-CNN-3AADD-GRAD-20-28METHOD-20NIL-20-28CL-CNN-3AVOLUME-20T-20T-20T-20T-29-29-29'></a>

- [method] **ADD-GRAD** *(VOL VOLUME) X Y D V*

    Adder for [`GRAD`][674e]

<a id='x-28CL-CNN-3ACLONE-20-28METHOD-20NIL-20-28CL-CNN-3AVOLUME-29-29-29'></a>

- [method] **CLONE** *(VOL VOLUME)*

<a id='x-28CL-CNN-3ACLONE-AND-ZERO-20-28METHOD-20NIL-20-28CL-CNN-3AVOLUME-29-29-29'></a>

- [method] **CLONE-AND-ZERO** *(VOL VOLUME)*

<a id='x-28CL-CNN-3AADD-20-28METHOD-20NIL-20-28CL-CNN-3AVOLUME-20CL-CNN-3AVOLUME-29-29-29'></a>

- [method] **ADD** *(V1 VOLUME) (V2 VOLUME)*

    Add value of `V2` in `V1`

<a id='x-28CL-CNN-3AADD-SCALED-20-28METHOD-20NIL-20-28CL-CNN-3AVOLUME-20CL-CNN-3AVOLUME-20T-29-29-29'></a>

- [method] **ADD-SCALED** *(V1 VOLUME) (V2 VOLUME) SCALE*

    Add SCALED value of `V2` in `V1`

<a id='x-28CL-CNN-3ASET-CONST-20-28METHOD-20NIL-20-28CL-CNN-3AVOLUME-20T-29-29-29'></a>

- [method] **SET-CONST** *(VOL VOLUME) V*

    Set a constant Value in `VOL`

<a id='x-28CL-CNN-3A-40NET-20MGL-PAX-3ASECTION-29'></a>

## 2 Networks

A [`NET`][2a65] is a very simple class that simply contains a list of [`LAYER`][b1b6]. When an example (in form of a `VOLUME`([`0`][d757] [`1`][dfd4])) is passed through the [`NET`][2a65], the [`NET`][2a65] simply iterates through all of its layers and propagates the example through each one in turn, and returns the result of the last [`LAYER`][b1b6]. Similarly, during backpropagation the Net calls the [`BACKWARD`][8779] function of each layer in turn to compute the gradient.

<a id='x-28CL-CNN-3ANET-20CLASS-29'></a>

- [class] **NET**

    Net manages a set of layers
    
    For now constraints: Simple linear order of layers, first layer input last layer a cost layer

<a id='x-28CL-CNN-3AADD-LAYER-20-28METHOD-20NIL-20-28CL-CNN-3ANET-20CL-CNN-3ALAYER-29-29-29'></a>

- [method] **ADD-LAYER** *(NET NET) (LAYER LAYER)*

    Push a `LAYER` in the end of the `NET` 

<a id='x-28CL-CNN-3AFORWARD-20-28METHOD-20NIL-20-28CL-CNN-3ANET-20CL-CNN-3AVOLUME-29-29-29'></a>

- [method] **FORWARD** *(NET NET) (VOL VOLUME)*

<a id='x-28CL-CNN-3ABACKWARD-20-28METHOD-20NIL-20-28CL-CNN-3ANET-29-29-29'></a>

- [method] **BACKWARD** *(NET NET)*

<a id='x-28CL-CNN-3A-40LAYERS-20MGL-PAX-3ASECTION-29'></a>

## 3 Layers

As mentioned, every Network ([`NET`][2a65]) is just a linear list of layers. Your first layer must be '[`INPUT`][65ae]' (in which you declare sizes of your input), your last layer must be a `LOSS` layer ('[`SOFTMAX`][8fc5]' or '[`SVM`][b5e0]' for classification, or 'REGRESSION' for regression). Every layer takes an input `VOLUME`([`0`][d757] [`1`][dfd4]) and produces a new output `VOLUME`([`0`][d757] [`1`][dfd4]), which is why I prefer to refer to them as transformers.

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
    (format t "score for class 0 is assigned: ~a~%"  (aref (cnn::w scores) 0))
    ))
```

 

<a id='x-28CL-CNN-3ALAYER-20CLASS-29'></a>

- [class] **LAYER**

<a id='x-28CL-CNN-3AINITIALIZE-20GENERIC-FUNCTION-29'></a>

- [generic-function] **INITIALIZE** *LAYER*

    post initialize the layer

<a id='x-28CL-CNN-3AFORWARD-20GENERIC-FUNCTION-29'></a>

- [generic-function] **FORWARD** *LAYER-OR-NET VOLUME &OPTIONAL IS-TRAINING*

    Forward the values of `VOLUME` in the [`LAYER`][b1b6] or the [`NET`][2a65]

<a id='x-28CL-CNN-3ABACKWARD-20GENERIC-FUNCTION-29'></a>

- [generic-function] **BACKWARD** *LAYER-OR-NET &OPTIONAL INDEX*

    Backpropagation

<a id='x-28CL-CNN-3AGET-PARAMS-AND-GRADS-20GENERIC-FUNCTION-29'></a>

- [generic-function] **GET-PARAMS-AND-GRADS** *LAYER-OR-NET*



<a id='x-28CL-CNN-3A-40INPUT-LAYER-20MGL-PAX-3ASECTION-29'></a>

### 3.1 Input Layers

<a id='x-28CL-CNN-3AINPUT-20CLASS-29'></a>

- [class] **INPUT** *[LAYER][b1b6]*

    Input Layer

<a id='x-28CL-CNN-3AFORWARD-20-28METHOD-20NIL-20-28CL-CNN-3AINPUT-20CL-CNN-3AVOLUME-29-29-29'></a>

- [method] **FORWARD** *(INPUT INPUT) (VOL VOLUME)*

<a id='x-28CL-CNN-3A-40FULLY-CONNECTED-LAYER-20MGL-PAX-3ASECTION-29'></a>

### 3.2 Fully connected Layers

<a id='x-28CL-CNN-3AFULLY-CONNECTED-20FUNCTION-29'></a>

- [function] **FULLY-CONNECTED** *&REST REST*

    Constructor for the class [`FULLY-CONNECTED`][618c]

<a id='x-28CL-CNN-3AFULLY-CONNECTED-20CLASS-29'></a>

- [class] **FULLY-CONNECTED** *[LAYER][b1b6]*

    Full connected Layer

<a id='x-28CL-CNN-3AFORWARD-20-28METHOD-20NIL-20-28CL-CNN-3AFULLY-CONNECTED-20CL-CNN-3AVOLUME-29-29-29'></a>

- [method] **FORWARD** *(INPUT FULLY-CONNECTED) (VOL VOLUME)*

<a id='x-28CL-CNN-3ABACKWARD-20-28METHOD-20NIL-20-28CL-CNN-3AFULLY-CONNECTED-29-29-29'></a>

- [method] **BACKWARD** *(INPUT FULLY-CONNECTED)*

<a id='x-28CL-CNN-3A-40LOSS-LAYERS-20MGL-PAX-3ASECTION-29'></a>

### 3.3 Loss Layers

 Layers that implement a loss. Currently these are the layers that 
can initiate a [`BACKWARD`][8779] pass. In future we probably want a more 
flexible system that can accomodate multiple losses to do multi-task
learning, and stuff like that. But for now, one of the layers in this
file must be the final layer in a Net.

<a id='x-28CL-CNN-3A-40SOFTMAX-LAYER-20MGL-PAX-3ASECTION-29'></a>

#### 3.3.1 Softmax Layers

This is a classifier, with N discrete classes from 0 to N-1
it gets a stream of N incoming numbers and computes the softmax
function (exponentiate and normalize to sum to 1 as probabilities should)

<a id='x-28CL-CNN-3ASOFTMAX-20CLASS-29'></a>

- [class] **SOFTMAX** *[LAYER][b1b6]*

    Implements Softmax loss

<a id='x-28CL-CNN-3AFORWARD-20-28METHOD-20NIL-20-28CL-CNN-3ASOFTMAX-20CL-CNN-3AVOLUME-29-29-29'></a>

- [method] **FORWARD** *(INPUT SOFTMAX) (VOL VOLUME)*

<a id='x-28CL-CNN-3ABACKWARD-20-28METHOD-20NIL-20-28CL-CNN-3ASOFTMAX-29-29-29'></a>

- [method] **BACKWARD** *(INPUT SOFTMAX)*

<a id='x-28CL-CNN-3A-40SVM-LAYER-20MGL-PAX-3ASECTION-29'></a>

#### 3.3.2 SVM Layers



<a id='x-28CL-CNN-3ASVM-20CLASS-29'></a>

- [class] **SVM** *[LAYER][b1b6]*

<a id='x-28CL-CNN-3AFORWARD-20-28METHOD-20NIL-20-28CL-CNN-3ASVM-20CL-CNN-3AVOLUME-29-29-29'></a>

- [method] **FORWARD** *(INPUT SVM) (VOL VOLUME)*

<a id='x-28CL-CNN-3ABACKWARD-20-28METHOD-20NIL-20-28CL-CNN-3ASVM-29-29-29'></a>

- [method] **BACKWARD** *(INPUT SVM)*

  [1d3d]: #x-28CL-CNN-3AVALUE-20-28METHOD-20NIL-20-28CL-CNN-3AVOLUME-20T-20T-20T-29-29-29 "(CL-CNN:VALUE (METHOD NIL (CL-CNN:VOLUME T T T)))"
  [29e6]: #x-28CL-CNN-3A-40FULLY-CONNECTED-LAYER-20MGL-PAX-3ASECTION-29 "Fully connected Layers"
  [2a65]: #x-28CL-CNN-3ANET-20CLASS-29 "(CL-CNN:NET CLASS)"
  [339e]: #x-28CL-CNN-3A-40SOFTMAX-LAYER-20MGL-PAX-3ASECTION-29 "Softmax Layers"
  [3b14]: #x-28CL-CNN-3A-40NET-20MGL-PAX-3ASECTION-29 "Networks"
  [42a3]: #x-28CL-CNN-3A-40INPUT-LAYER-20MGL-PAX-3ASECTION-29 "Input Layers"
  [458c]: #x-28CL-CNN-3A-40VOLUMES-20MGL-PAX-3ASECTION-29 "Volumes"
  [618c]: #x-28CL-CNN-3AFULLY-CONNECTED-20CLASS-29 "(CL-CNN:FULLY-CONNECTED CLASS)"
  [65ae]: #x-28CL-CNN-3AINPUT-20CLASS-29 "(CL-CNN:INPUT CLASS)"
  [674e]: #x-28CL-CNN-3AGRAD-20-28METHOD-20NIL-20-28CL-CNN-3AVOLUME-20T-20T-20T-29-29-29 "(CL-CNN:GRAD (METHOD NIL (CL-CNN:VOLUME T T T)))"
  [85dc]: #x-28CL-CNN-3A-40LAYERS-20MGL-PAX-3ASECTION-29 "Layers"
  [8779]: #x-28CL-CNN-3ABACKWARD-20GENERIC-FUNCTION-29 "(CL-CNN:BACKWARD GENERIC-FUNCTION)"
  [8fc5]: #x-28CL-CNN-3ASOFTMAX-20CLASS-29 "(CL-CNN:SOFTMAX CLASS)"
  [a784]: #x-28CL-CNN-3A-40LOSS-LAYERS-20MGL-PAX-3ASECTION-29 "Loss Layers"
  [aeeb]: #x-28CL-CNN-3A-40SVM-LAYER-20MGL-PAX-3ASECTION-29 "SVM Layers"
  [b1b6]: #x-28CL-CNN-3ALAYER-20CLASS-29 "(CL-CNN:LAYER CLASS)"
  [b5e0]: #x-28CL-CNN-3ASVM-20CLASS-29 "(CL-CNN:SVM CLASS)"
  [d757]: #x-28CL-CNN-3AVOLUME-20FUNCTION-29 "(CL-CNN:VOLUME FUNCTION)"
  [dfd4]: #x-28CL-CNN-3AVOLUME-20CLASS-29 "(CL-CNN:VOLUME CLASS)"
