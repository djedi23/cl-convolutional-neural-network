(in-package #:cl-cnn)

(mgl-pax:defsection @main (:title "CL Convolutional Neural Networks")
  "## Installation

```lisp
(ql:quickload :cl-cnn)
```

Translate from:
https://github.com/karpathy/convnetjs"
  (@volumes mgl-pax:section)
  )

(mgl-pax:defsection @volumes (:title "Volumes")
  (volume class)
  (value (method () (volume t t t)))
  (setf-value (method () (volume t t t t)))
  (add-value (method () (volume t t t t)))
  (grad (method () (volume t t t)))
  (setf-grad (method () (volume t t t t)))
  (add-grad (method () (volume t t t t)))
  )



(defun make-readme.md ()
  (with-open-file (s (asdf:system-relative-pathname :cl-cnn "README.md")
		     :direction :output :if-exists :supersede)
    (mgl-pax:document @main :stream s))
  (update-wiki))


(defun update-wiki ()
  (let ((sections (list
		   @volumes
		   @main
		   )))
    (mgl-pax:document
     ;; This could be a list of objects (typically sections), if you
     ;; want to generate documentation for related libraries with
     ;; cross-links.
     @main
     :pages (loop for section in sections
	       collect (section-to-page-spec section))
     )))

(defun section-to-filename (section)
  (asdf:system-relative-pathname
   :cl-cnn
   (format nil "../cl-cnn.wiki/~a.md"
           (string-downcase (subseq (symbol-name (mgl-pax:section-name section)) 1)))))

(defun section-to-page-spec (section &key
				       (open-args '(:if-does-not-exist :create
						    :if-exists :supersede
						    :ensure-directories-exist t)))
  `(:objects
    (,section)
    :output (,(section-to-filename section) ,@open-args)
    :uri-fragment ,(format nil "https://git.valvassori.info/djedi/cl-cnn/wikis/~a" (string-downcase (subseq (symbol-name (mgl-pax:section-name section)) 1)))
    ))
