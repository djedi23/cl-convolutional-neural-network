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
    the data. c is optionally a value to initialize the volume
    with. If c is missing, fills the Vol with random numbers.

  [458c]: #x-28CL-CNN-3A-40VOLUMES-20MGL-PAX-3ASECTION-29 "Volumes"
