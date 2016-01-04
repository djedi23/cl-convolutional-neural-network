(in-package #:cl-cnn)

(declaim (optimize (debug 3)))

(defgeneric parse-definition (net def-function def))

(defmethod parse-definition ((net symbol) f def)
  `((add-layer ,net ,def)))

(defmethod parse-definition ((net symbol) (f (eql 'svm)) def)
  `((add-layer ,net ,def)
    (add-layer ,net (fully-connected))))



(defun parse-net-definitions (net defs)
 (print (concatenate 'list (print
	       (mapcar #'(lambda (def)
			   (parse-definition net (car def) def))
		       defs)))))

(defmacro define-net (name &body body)
  (let ((var (gensym (symbol-name name))))
    `(let ((,var (net)))
       ,@(parse-net-definitions var body)
       (setq ,name ,var)
       )))
