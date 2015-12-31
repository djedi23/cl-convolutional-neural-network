<a id='x-28CL-CNN-3A-40MAIN-20MGL-PAX-3ASECTION-29'></a>

# CL Convolutional Neural Networks

## Table of Contents

- [1 Volumes][458c]
- [2 Networks][3b14]
- [3 Layers][85dc]
    - [3.1 Input Layers][42a3]
    - [3.2 Input Layers][29e6]

###### \[in package CL-CNN\]
## Installation

```lisp
(ql:quickload :cl-cnn)
```

Translate from:
https://github.com/karpathy/convnetjs

<a id='x-28CL-CNN-3A-40VOLUMES-20MGL-PAX-3ASECTION-29'></a>

## 1 Volumes

<a id='x-28CL-CNN-3AVOLUME-20CLASS-29'></a>

- [class] **VOLUME**

    `VOLUME` is the basic building block of all data in a [`NET`][2a65].
    It is essentially just a 3D volume of numbers, with a
    width (SX), height (SY), and depth (`DEPTH`).
    it is used to hold data for all filters, all volumes,
    all weights, and also stores all gradients w.r.t. 
    the data. C is optionally a value to initialize the `VOLUME`
    with. If C is missing, fills the `VOLUME` with random numbers.

<a id='x-28CL-CNN-3AVALUE-20-28METHOD-20NIL-20-28CL-CNN-3AVOLUME-20T-20T-20T-29-29-29'></a>

- [method] **VALUE** *(VOL VOLUME) X Y D*

    Read the value at `X` `Y` `D` of the [`VOLUME`][dfd4].

<a id='x-28CL-CNN-3A-3ASETF-VALUE-20-28METHOD-20NIL-20-28CL-CNN-3AVOLUME-20T-20T-20T-20T-29-29-29'></a>

- [method] **SETF-VALUE** *(VOL VOLUME) X Y D V*

    Setter for [`VALUE`][1d3d]

<a id='x-28CL-CNN-3AADD-VALUE-20-28METHOD-20NIL-20-28CL-CNN-3AVOLUME-20T-20T-20T-20T-29-29-29'></a>

- [method] **ADD-VALUE** *(VOL VOLUME) X Y D V*

    Adder for [`VALUE`][1d3d]

<a id='x-28CL-CNN-3AGRAD-20-28METHOD-20NIL-20-28CL-CNN-3AVOLUME-20T-20T-20T-29-29-29'></a>

- [method] **GRAD** *(VOL VOLUME) X Y D*

    Read the grad at `X` `Y` `D` of the [`VOLUME`][dfd4].

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

<a id='x-28CL-CNN-3ANET-20CLASS-29'></a>

- [class] **NET**

    Net manages a set of layers
    
    For now constraints: Simple linear order of layers, first layer input last layer a cost layer

<a id='x-28CL-CNN-3A-40LAYERS-20MGL-PAX-3ASECTION-29'></a>

## 3 Layers

<a id='x-28CL-CNN-3ALAYER-20CLASS-29'></a>

- [class] **LAYER**

<a id='x-28CL-CNN-3AFORWARD-20GENERIC-FUNCTION-29'></a>

- [generic-function] **FORWARD** *LAYER-OR-NET VOLUME &OPTIONAL IS-TRAINING*

    Forward the values of `VOLUME` in the [`LAYER`][b1b6] or the [`NET`][2a65]

<a id='x-28CL-CNN-3ABACKWARD-20GENERIC-FUNCTION-29'></a>

- [generic-function] **BACKWARD** *LAYER-OR-NET*

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

### 3.2 Input Layers

<a id='x-28CL-CNN-3AFULLY-CONNECTED-20CLASS-29'></a>

- [class] **FULLY-CONNECTED** *[LAYER][b1b6]*

    Full connected Layer

<a id='x-28CL-CNN-3AFORWARD-20-28METHOD-20NIL-20-28CL-CNN-3AFULLY-CONNECTED-20CL-CNN-3AVOLUME-29-29-29'></a>

- [method] **FORWARD** *(INPUT FULLY-CONNECTED) (VOL VOLUME)*

  [1d3d]: #x-28CL-CNN-3AVALUE-20-28METHOD-20NIL-20-28CL-CNN-3AVOLUME-20T-20T-20T-29-29-29 "(CL-CNN:VALUE (METHOD NIL (CL-CNN:VOLUME T T T)))"
  [29e6]: #x-28CL-CNN-3A-40FULLY-CONNECTED-LAYER-20MGL-PAX-3ASECTION-29 "Input Layers"
  [2a65]: #x-28CL-CNN-3ANET-20CLASS-29 "(CL-CNN:NET CLASS)"
  [3b14]: #x-28CL-CNN-3A-40NET-20MGL-PAX-3ASECTION-29 "Networks"
  [42a3]: #x-28CL-CNN-3A-40INPUT-LAYER-20MGL-PAX-3ASECTION-29 "Input Layers"
  [458c]: #x-28CL-CNN-3A-40VOLUMES-20MGL-PAX-3ASECTION-29 "Volumes"
  [674e]: #x-28CL-CNN-3AGRAD-20-28METHOD-20NIL-20-28CL-CNN-3AVOLUME-20T-20T-20T-29-29-29 "(CL-CNN:GRAD (METHOD NIL (CL-CNN:VOLUME T T T)))"
  [85dc]: #x-28CL-CNN-3A-40LAYERS-20MGL-PAX-3ASECTION-29 "Layers"
  [b1b6]: #x-28CL-CNN-3ALAYER-20CLASS-29 "(CL-CNN:LAYER CLASS)"
  [dfd4]: #x-28CL-CNN-3AVOLUME-20CLASS-29 "(CL-CNN:VOLUME CLASS)"
