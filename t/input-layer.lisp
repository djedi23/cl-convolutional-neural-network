(in-package :cl-cnn-test)

(plan nil)

(diag "Input Layer")
(let ((input (make-instance 'input)))
  (ok input)
  )

(diag "Input Layer forward")
(let ((input (make-instance 'input))
      (vol (make-instance 'volume :sx 2 :sy 3 :depth 4 :c 10.0)))
  (ok input)
  (is (class-name (class-of (forward input vol))) 'VOLUME)
  )


(finalize)
