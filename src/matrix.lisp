;;
(defpackage matrix
  (:use :cl)
  (:export :make-matrix :set-xy :get-xy
   :matrix-p :equals? :matrix-dim :mul :mul-tup :identity4
   :transpose))

(in-package :matrix)

(defparameter identity4 #2A ((1.0d0 0.0d0 0.0d0 0.0d0)
                            (0.0d0 1.0d0 0.0d0 0.0d0)
                            (0.0d0 0.0d0 1.0d0 0.0d0)
                            (0.0d0 0.0d0 0.0d0 1.0d0)))
(defun matrix-p (mat)
  (and (= (array-rank mat) 2)
       (= (array-dimension mat 0) (array-dimension mat 1))))

(defmacro make-matrix (dim)
  `(let ((d ,dim))
     (make-array (list d d) :initial-element 0.0d0 :element-type 'double-float)))

(defmacro set-xy (mat row col val)
  `(setf (aref ,mat ,row ,col) ,val))

(defmacro get-xy (mat row col)
  `(aref ,mat ,row ,col))

(defmacro matrix-dim (mat)
  `(array-dimension ,mat 0))

(defmacro equals? (m1 m2)
  `(equalp ,m1 ,m2))

(defun mul (m1 m2)
  (let ((result (make-matrix (matrix-dim m1))))
    (dotimes (row (matrix-dim m1))
      (dotimes (col (matrix-dim m1))
        (let ((val 0))
          (dotimes (i (matrix-dim m1))
            (setf val (+ val (* (get-xy m1 row i) (get-xy m2 i col))))
            )
          (set-xy result row col val)
          )
        )      
      )
    result)
  )

(defun mul-tup (m1 tup1)
  (let ((result (tuples:make-tuple 0.0d0 0.0d0 0.0d0 0.0d0)))
    (dotimes (row (matrix-dim m1))
      (let ((val 0))
        (dotimes (i (matrix-dim m1))
          (setf val (+ val (* (get-xy m1 row i) (aref tup1 i)))))
        (setf (aref result row) val)))
    result))

(defun transpose (m1)
  (let ((result (make-matrix (matrix-dim m1))))
    (dotimes (row (matrix-dim m1))
      (dotimes (col (matrix-dim m1))
        (set-xy result col row (get-xy m1 row col))
        )      
      )
    result)
  )
