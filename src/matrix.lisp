(defpackage matrix
  (:use :cl :colors)
  (:export :matrix :make-matrix :matrix-din :matrix-data 
   :matrix-p :init-matrix :at :put))

(in-package :matrix)

(defstruct matrix (dim 4) (data nil))

(defun init-matrix (dim)
  (make-matrix :dim dim :data (make-array (list dim dim) :initial-element 0.0))
  )

(defun at (m x y)
  (aref (matrix-data m) y x))

(defun put (m x y v)
  (setf (aref (matrix-data m) y x) v))


