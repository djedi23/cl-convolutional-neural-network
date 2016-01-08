(in-package #:cl-cnn)

(declaim (optimize (debug 3)))

(defun is-layer (class-name)
  (let ((class (find-class class-name nil))
	(layer-class (find-class 'layer)))
    (if (null class) nil
	(member layer-class (sb-mop:class-precedence-list class))
	)))


(defgeneric parse-layer-definition (net layer-name def)
  (:documentation "Parse a layer définition and return a list of add layer"))

(defmethod parse-layer-definition ((net symbol) layer def)
  (if (is-layer layer)
      `((add-layer ,net ,def))
      (list (flatten0 (mapcar #'(lambda (item)
				  (if (atom item)
				      (list item)
				      (parse-layer-definition net (car item) item)))
			      def)))
      ))

(defmethod parse-layer-definition ((net symbol) (layer (eql 'svm)) def)
  (destructuring-bind (layer-name &key num-classes  &allow-other-keys) def
    `((add-layer ,net ,def)
      (add-layer ,net (fully-connected ,@(when num-classes `(:num-neurons ,num-classes)))))))

(defmethod parse-layer-definition ((net symbol) (layer (eql 'softmax)) def)
  (destructuring-bind (layer-name &key num-classes  &allow-other-keys) def
    `((add-layer ,net ,def)
      (add-layer ,net (fully-connected ,@(when num-classes `(:num-neurons ,num-classes)))))))

#+to-be-implemented
(defmethod parse-layer-definition ((net symbol) (layer (eql 'regression)) def)
  (destructuring-bind (layer-name &key num-neurons  &allow-other-keys) def
    `((add-layer ,net ,def)
      (add-layer ,net (fully-connected ,@(when num-classes `(:num-neurons ,num-neurons)))))))


(defmethod parse-layer-definition ((net symbol) (layer (eql 'fully-connected)) def)
  (destructuring-bind (layer-name &key activation &allow-other-keys) def
    `((add-layer ,net ,def)
      ,(when activation
	     `(add-layer ,net ,(if (listp activation) activation `(,activation))  )))))


;; TODO ajouter le dropout

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
