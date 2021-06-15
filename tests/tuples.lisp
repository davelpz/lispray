(defpackage lispray/tests/tuples
  (:use :cl
   :tuples
        :rove))
(in-package :lispray/tests/tuples)

;; NOTE: To run this test file, execute `(asdf:test-system :tuples)' in your Lisp.

(deftest test-tuples
  (let ((tup (make-tuple 4.3d0 -4.2d0 3.1d0 1.0d0)))
    (testing "should be true"
      (ok (= (tuples:get-x tup) 4.3d0)))
    (testing "should be true"
      (ok (= (tuples:get-y tup) -4.2d0)))
    (testing "should be true"
      (ok (= (tuples:get-z tup) 3.1d0)))
    (testing "should be true"
      (ok (= (tuples:get-w tup) 1.0d0)))
    (testing "should be true"
      (ok (point-p tup)))
    (testing "should be true"
      (ng (vector-p tup)))
    )
  (let ((tup (make-tuple 4.3d0 -4.2d0 3.1d0 0.0d0)))
    (testing "should be true"
      (ok (= (tuples:get-x tup) 4.3d0)))
    (testing "should be true"
      (ok (= (tuples:get-y tup) -4.2d0)))
    (testing "should be true"
      (ok (= (tuples:get-z tup) 3.1d0)))
    (testing "should be true"
      (ok (= (tuples:get-w tup) 0.0d0)))
    (testing "should be true"
      (ng (point-p tup)))
    (testing "should be true"
      (ok (vector-p tup)))
    )
  (testing "should be equal"
    (ok (equalp (make-vector 4.0d0 -4.0d0 3.0d0) (make-tuple 4.0d0 -4.0d0 3.0d0 0.0d0))))
  (testing "should be equal"
    (ok (equalp (make-point 4.0d0 -4.0d0 3.0d0) (make-tuple 4.0d0 -4.0d0 3.0d0 1.0d0))))
  )
