(defpackage lispray/tests/ray
  (:use :cl
   :ray
        :rove))
(in-package :lispray/tests/ray)

;; NOTE: To run this test file, execute `(asdf:test-system :tuples)' in your Lisp.
(defmacro df (val) 
  (let ((nv (coerce val 'double-float))) nv))

(defun mtrx (m)
  (let ((ans (matrix:make-matrix (matrix:matrix-dim m))))
    (dotimes (row (matrix:matrix-dim m))
      (dotimes (col (matrix:matrix-dim m))
        (matrix:set-xy ans row col (coerce (matrix:get-xy m row col) 'double-float))
        ))
    ans))

(defun tup-eql (t1 t2)
  (and (< (abs (- (tuples:get-x t1) (tuples:get-x t2))) 0.0000001d0)
       (< (abs (- (tuples:get-y t1) (tuples:get-y t2))) 0.0000001d0)
       (< (abs (- (tuples:get-z t1) (tuples:get-z t2))) 0.0000001d0)
       (< (abs (- (tuples:get-w t1) (tuples:get-w t2))) 0.0000001d0)))

(deftest test-ray
  (let* ((origin (tuples:make-point 1.0d0 2.0d0 3.0d0))
         (direction (tuples:make-vector 4.0d0 5.0d0 6.0d0))
         (r (make-ray origin direction)))
    (testing "testing accessors"
      (ok (tup-eql (origin r) origin))
      (ok (tup-eql (direction r) direction))
      )
    )
  (let* ((origin (tuples:make-point 2.0d0 3.0d0 4.0d0))
         (direction (tuples:make-vector 1.0d0 0.0d0 0.0d0))
         (r (make-ray origin direction)))
    (testing "testing accessors"
      (ok (tup-eql (ray-position r 0.0d0) (tuples:make-point 2.0d0 3.0d0 4.0d0)))
      (ok (tup-eql (ray-position r 1.0d0) (tuples:make-point 3.0d0 3.0d0 4.0d0)))
      (ok (tup-eql (ray-position r -1.0d0) (tuples:make-point 1.0d0 3.0d0 4.0d0)))
      (ok (tup-eql (ray-position r 2.5d0) (tuples:make-point 4.5d0 3.0d0 4.0d0)))
      )
    )
  )
