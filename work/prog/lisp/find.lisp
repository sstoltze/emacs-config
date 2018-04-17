;; Build an internal representation of a directory/file
                                        ; Struct? List?
(defpackage :cl-dir
  (:use :common-lisp))

(in-package :cl-dir)

;; For future reference
(directory "../lisp/*.*") ; lister filer i ../lisp
(directory "../*") ; lister dirs i ..

(defclass dir-node ()
  ((name :initarg :name)
   (path :initarg :path)
   (size :initarg :size :initform 0)))
(defclass file (dir-node)
  ())
(defclass dir (dir-node)
  ((contents :initarg :contents :initform (list))))
  
(defmethod print-object ((d dir-node) s)
  (format s "~S/~S" (slot-value d 'path) (slot-value d 'name)))

