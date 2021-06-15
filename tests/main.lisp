(defpackage lispray/tests/main
  (:use :cl
        :lispray
        :rove))
(in-package :lispray/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :lispray)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
