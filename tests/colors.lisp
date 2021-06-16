(defpackage lispray/tests/colors
  (:use :cl
   :colors
        :rove))
(in-package :lispray/tests/colors)

;; NOTE: To run this test file, execute `(asdf:test-system :tuples)' in your Lisp.

(deftest test-colors
  (let ((col (make-color :red -0.5d0 :green 0.4d0 :blue 1.7d0)))
    (testing "should be true"
      (ok (= (color-red col) -0.5d0)))
    (testing "should be true"
      (ok (= (color-green col) 0.4d0)))
    (testing "should be true"
      (ok (= (color-blue col) 1.7d0)))
    (testing "should be true"
      (ok (color-p col)))
    )
  )

(deftest test-colors-operations
  (let ((c1 (make-color :red 0.9d0 :green 0.6d0 :blue 0.75d0))
        (c2 (make-color :red 0.7d0 :green 0.1d0 :blue 0.25d0)))
    (testing "should be true"
      (ok (equalp (add c1 c2) (make-color :red 1.6d0 :green 0.7d0 :blue 1.0d0))))
    )
  (let ((c1 (make-color :red 0.9d0 :green 0.6d0 :blue 0.75d0))
        (c2 (make-color :red 0.7d0 :green 0.1d0 :blue 0.25d0)))
    (testing "should be true"
      (ok (equalp (sub c1 c2) (make-color :red 0.20000000000000007d0 :green 0.5d0 :blue 0.5d0))))
    )
  (let ((c1 (make-color :red 0.2d0 :green 0.3d0 :blue 0.4d0)))
    (testing "should be true"
      (ok (equalp (mul c1 2.0d0) (make-color :red 0.4d0 :green 0.6d0 :blue 0.8d0))))
    )
  (let ((c1 (make-color :red 1.0d0 :green 0.2d0 :blue 0.4d0))
        (c2 (make-color :red 0.9d0 :green 1.0d0 :blue 0.1d0)))
    (testing "should be true"
      (ok (equalp (mul c1 c2) (make-color :red 0.9d0 :green 0.2d0 :blue 0.04000000000000001d0))))
    )  
  )
