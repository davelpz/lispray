(defpackage lispray/tests/shapes
  (:use :cl
   :shapes
        :rove))
(in-package :lispray/tests/shapes)

(defun tup-eql (t1 t2)
  (and (< (abs (- (tuples:get-x t1) (tuples:get-x t2))) 0.0000001d0)
       (< (abs (- (tuples:get-y t1) (tuples:get-y t2))) 0.0000001d0)
       (< (abs (- (tuples:get-z t1) (tuples:get-z t2))) 0.0000001d0)
       (< (abs (- (tuples:get-w t1) (tuples:get-w t2))) 0.0000001d0)))

(deftest test-shape
  (let* ((s (make-sphere))
         (r (ray:make-ray (tuples:make-point 0.0d0 0.0d0 -5.0d0) (tuples:make-vector 0.0d0 0.0d0 1.0d0)))
         (xs (intersect s r)))
    (testing "testing accessors"
      (ok (tup-eql (ray:origin r) (tuples:make-point 0.0d0 0.0d0 -5.0d0)))
      (ok (tup-eql (ray:direction r) (tuples:make-vector 0.0d0 0.0d0 1.0d0)))
      )
    ))
