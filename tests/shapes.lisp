(defpackage lispray/tests/shapes
  (:use :cl
   :shapes
        :rove))
(in-package :lispray/tests/shapes)

(deftest test-ray
  (let* ((origin (tuples:make-point 1.0d0 2.0d0 3.0d0))
         (direction (tuples:make-vector 4.0d0 5.0d0 6.0d0))
         (r (make-ray origin direction)))
    (testing "testing accessors"
      (ok (tup-eql (origin r) origin))
      (ok (tup-eql (direction r) direction))
      )
    ))
