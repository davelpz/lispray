
(defpackage matrix
  (:use :cl)
  (:export :matrix :make-matrix :matrix-dim :matrix-data 
           :matrix-p :init-matrix :at :put :equals? :mul))

(in-package :matrix)

(defstruct matrix (dim 4) (data nil))

(defun init-matrix (dim)
  (make-matrix :dim dim :data (make-array (list dim dim) :initial-element 0.0))
  )

(defun at (m row col)
  (aref (matrix-data m) row col))

(defun put (m row col v)
  (setf (aref (matrix-data m) row col) v))

(defun equals? (m1 m2) (equalp m1 m2))

(defun mul-mat (m1 m2)
  (let ((result (init-matrix (matrix-dim m1))))
    (dotimes (row (matrix-dim m1))
      (dotimes (col (matrix-dim m1))
        (let ((val 0))
          (dotimes (i (matrix-dim m1))
            (setf val (+ val (* (at m1 row i) (at m2 i col))))
            )
          (put result row col val)
          )
        )      
      )
    result)
  )

(defun mul-mat-tup (m1 m2)
  (let ((result (tuples:make-tuple)))
    (dotimes (row (matrix-dim m1))
      (let ((val 0))
        (dotimes (i (matrix-dim m1))
          (setf val (+ val (* (at m1 row i) (tuples:at m2 i))))
          )
        (tuples:put result row val)
        )    
      )
    result)
  )

(defun mul (m1 m2)
  (cond ((and (matrix-p m1) (matrix-p m2)) (mul-mat m1 m2))
        ((and (matrix-p m1) (tuples:tuple-p m2)) (mul-mat-tup m1 m2))
        )
  )
