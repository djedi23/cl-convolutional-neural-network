(in-package #:cl-cnn)

(defclass volume ()
  ()
  (:documentation "Vol is the basic building block of all data in a net.
  it is essentially just a 3D volume of numbers, with a
  width (sx), height (sy), and depth (depth).
  it is used to hold data for all filters, all volumes,
  all weights, and also stores all gradients w.r.t. 
  the data. c is optionally a value to initialize the volume
  with. If c is missing, fills the Vol with random numbers."))
