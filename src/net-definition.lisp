(in-package #:cl-cnn)

(declaim (optimize (debug 3)))

(defun parse-net-definition (net defs)
  (mapcar #'(lambda (def)
	      `(add-layer ,net ,def))
	  defs))

(defmacro define-net (name &body body)
  (let ((var (gensym (symbol-name name))))
    `(let ((,var (net)))
       ,@(parse-net-definition var body)
       (setq ,name ,var)
       )))
