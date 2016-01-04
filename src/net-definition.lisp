(in-package #:cl-cnn)

(declaim (optimize (debug 3)))

(defgeneric parse-layer-definition (net layer-name def)
(:documentation "Parse a layer définition and return a list of add layer"))

(defmethod parse-layer-definition ((net symbol) layer def)
  `((add-layer ,net ,def)))

(defmethod parse-layer-definition ((net symbol) (layer (eql 'svm)) def)
 (destructuring-bind (layer-name &key num-classes  &allow-other-keys) def
  `((add-layer ,net ,def)
    (add-layer ,net (fully-connected ,@(when num-classes `(:num-classes ,num-classes)))))))

(defun parse-net-definitions (net defs)
  (flatten0 `(,@(mapcar #'(lambda (def)
			   (parse-layer-definition net (car def) def))
		       defs))))

(defmacro define-net ((&key name) &body body)
  (let ((var (gensym (if name (symbol-name name) "NET"))))
    `(let ((,var (net)))
       ,@(parse-net-definitions var body)
        ,(if name
           `(setq ,name ,var)
           var)
       )))
