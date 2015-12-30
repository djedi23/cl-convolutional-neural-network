<a id='x-28CL-CNN-3A-40MAIN-20MGL-PAX-3ASECTION-29'></a>

# CL Convolutional Neural Networks

## Table of Contents

- [1 Volumes][458c]

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

    `VOLUME` is the basic building block of all data in a NET.
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

  [1d3d]: #x-28CL-CNN-3AVALUE-20-28METHOD-20NIL-20-28CL-CNN-3AVOLUME-20T-20T-20T-29-29-29 "(CL-CNN:VALUE (METHOD NIL (CL-CNN:VOLUME T T T)))"
  [458c]: #x-28CL-CNN-3A-40VOLUMES-20MGL-PAX-3ASECTION-29 "Volumes"
  [dfd4]: #x-28CL-CNN-3AVOLUME-20CLASS-29 "(CL-CNN:VOLUME CLASS)"
