
(defpackage matrix
  (:use :cl)
  (:export :make-matrix :set-xy :get-xy
           :matrix-p :equals? :mul))

(in-package :matrix)

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

(defmacro equals? (m1 m2)
  `(equalp ,m1 ,m2))

;; (defun mul-mat (m1 m2)
;;   (let ((result (init-matrix (matrix-dim m1))))
;;     (dotimes (row (matrix-dim m1))
;;       (dotimes (col (matrix-dim m1))
;;         (let ((val 0))
;;           (dotimes (i (matrix-dim m1))
;;             (setf val (+ val (* (at m1 row i) (at m2 i col))))
;;             )
;;           (put result row col val)
;;           )
;;         )      
;;       )
;;     result)
;;   )

;; (defun mul-mat-tup (m1 m2)
;;   (let ((result (tuples:make-tuple)))
;;     (dotimes (row (matrix-dim m1))
;;       (let ((val 0))
;;         (dotimes (i (matrix-dim m1))
;;           (setf val (+ val (* (at m1 row i) (tuples:at m2 i))))
;;           )
;;         (tuples:put result row val)
;;         )    
;;       )
;;     result)
;;   )

;; (defun mul (m1 m2)
;;   (cond ((and (matrix-p m1) (matrix-p m2)) (mul-mat m1 m2))
;;         ((and (matrix-p m1) (tuples:tuple-p m2)) (mul-mat-tup m1 m2))
;;         )
;;   )
