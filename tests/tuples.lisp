(defpackage lispray/tests/tuples
  (:use :cl
   :tuples
        :rove))
(in-package :lispray/tests/tuples)

;; NOTE: To run this test file, execute `(asdf:test-system :tuples)' in your Lisp.

(deftest test-tuples
  (let ((tup (make-tuple 4.3d0 -4.2d0 3.1d0 1.0d0)))
    (testing "should be true"
      (ok (= (get-x tup) 4.3d0)))
    (testing "should be true"
      (ok (= (get-y tup) -4.2d0)))
    (testing "should be true"
      (ok (= (get-z tup) 3.1d0)))
    (testing "should be true"
      (ok (= (get-w tup) 1.0d0)))
    (testing "should be true"
      (ok (point-p tup)))
    (testing "should be true"
      (ng (vector-p tup)))
    )
  (let ((tup (make-tuple 4.3d0 -4.2d0 3.1d0 0.0d0)))
    (testing "should be true"
      (ok (= (get-x tup) 4.3d0)))
    (testing "should be true"
      (ok (= (get-y tup) -4.2d0)))
    (testing "should be true"
      (ok (= (get-z tup) 3.1d0)))
    (testing "should be true"
      (ok (= (get-w tup) 0.0d0)))
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

(deftest test-tuples-operations
  (let ((a1 (make-tuple 3.0d0 -2.0d0 5.0d0 1.0d0))
        (a2 (make-tuple -2.0d0 3.0d0 1.0d0 0.0d0)))
    (testing "should be true"
      (ok (equalp (add a1 a2) (make-tuple 1.0d0 1.0d0 6.0d0 1.0d0))))
    )
  (let ((p1 (make-point 3.0d0 2.0d0 1.0d0))
        (p2 (make-point 5.0d0 6.0d0 7.0d0)))
    (testing "should be true"
      (ok (equalp (sub p1 p2) (make-vector -2.0d0 -4.0d0 -6.0d0))))
    )
  (let ((p (make-point 3.0d0 2.0d0 1.0d0))
        (v (make-vector 5.0d0 6.0d0 7.0d0)))
    (testing "should be true"
      (ok (equalp (sub p v) (make-point -2.0d0 -4.0d0 -6.0d0))))
    )
  (let ((v1 (make-vector 3.0d0 2.0d0 1.0d0))
        (v2 (make-vector 5.0d0 6.0d0 7.0d0)))
    (testing "should be true"
      (ok (equalp (sub v1 v2) (make-vector -2.0d0 -4.0d0 -6.0d0))))
    )
  )
