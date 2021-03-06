;;;; package.lisp

(defpackage #:cl-cnn
    (:nicknames :cnn)
  (:use #:cl)
  (:export
   volume
   value
   add-value
   grad
   add-grad
   clone
   clone-and-zero
   add
   add-scaled
   const
   set-const

   net
   add-layer
   define-net
   
   forward
   backward
   get-params-and-grads
   
   input
   layer
   fully-connected
   relu
   softmax
   svm
   ))

