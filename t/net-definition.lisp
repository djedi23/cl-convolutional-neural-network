(in-package :cl-cnn-test)

(plan nil)

#+disabled(print (macroexpand
 '(cnn::define-net (:name example)
  (input :out-sx 1 :out-sy 1 :out-depth 2)
  (cnn::svm :num-classes 22)
  )))

#-disable(progn
(cnn::define-net (:name example)
  (input :out-sx 1 :out-sy 1 :out-depth 2)
  (cnn::svm :num-classes 22))
(is (fill-pointer (cnn::layers example)) 3)
)

(finalize)
