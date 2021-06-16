(defpackage lispray/tests/matrix
  (:use :cl
   :matrix
        :rove))
(in-package :lispray/tests/matrix)

;; NOTE: To run this test file, execute `(asdf:test-system :tuples)' in your Lisp.

(deftest test-matrix
  (let ((mat #2A((1.0d0 2.0d0 3.0d0 4.0d0)
                (5.5d0 6.5d0 7.5d0 8.5d0)
                (9.0d0 10.0d0 11.0d0 12.0d0)
                (13.5d0 14.5d0 15.5d0 16.5d0))))
    (testing "should be true"
      (ok (= (get-xy mat 0 0) 1.0d0)))
    (testing "should be true"
      (ok (= (get-xy mat 0 3) 4.0d0)))
    (testing "should be true"
             (ok (= (get-xy mat 1 0) 5.5d0)))
    (testing "should be true"
             (ok (= (get-xy mat 1 2) 7.5d0)))
    (testing "should be true"
             (ok (= (get-xy mat 2 2) 11.0d0)))
    (testing "should be true"
             (ok (= (get-xy mat 3 0) 13.5d0)))
    (testing "should be true"
             (ok (= (get-xy mat 3 2) 15.5d0)))
    )
  (let ((mat #2A((-3.0d0 5.0d0)
                 (1.0d0 -2.0d0))))
    (testing "should be true"
      (ok (= (get-xy mat 0 0) -3.0d0)))
    (testing "should be true"
      (ok (= (get-xy mat 0 1) 5.0d0)))
    (testing "should be true"
      (ok (= (get-xy mat 1 0) 1.0d0)))
    (testing "should be true"
      (ok (= (get-xy mat 1 1) -2.0d0)))
    )
  (let ((mat #2A((-3.0d0 5.0d0 0.0d0)
                 (1.0d0 -2.0d0 -7.0d0)
                 (0.0d0 1.0d0 1.0d0))))
    (testing "should be true"
      (ok (= (get-xy mat 0 0) -3.0d0)))
    (testing "should be true"
      (ok (= (get-xy mat 1 1) -2.0d0)))
    (testing "should be true"
      (ok (= (get-xy mat 2 2) 1.0d0)))
    )
  (let ((mat1 #2A((1.0d0 2.0d0 3.0d0 4.0d0)
                  (5.0d0 6.0d0 7.0d0 8.0d0)
                  (9.0d0 8.0d0 7.0d0 6.0d0)
                  (5.0d0 4.0d0 3.0d0 2.0d0)))
        (mat2 #2A((2.0d0 3.0d0 4.0d0 5.0d0)
                  (6.0d0 7.0d0 8.0d0 9.0d0)
                  (8.0d0 7.0d0 6.0d0 5.0d0)
                  (4.0d0 3.0d0 2.0d0 1.0d0))))
    (testing "should be true"
      (ng (equals? mat1 mat2)))

    )
  (let ((mat1 #2A((1.0d0 2.0d0 3.0d0 4.0d0)
                  (5.0d0 6.0d0 7.0d0 8.0d0)
                  (9.0d0 8.0d0 7.0d0 6.0d0)
                  (5.0d0 4.0d0 3.0d0 2.0d0)))
        (mat2 #2A((-2.0d0 1.0d0 2.0d0 3.0d0)
                  (3.0d0 2.0d0 1.0d0 -1.0d0)
                  (4.0d0 3.0d0 6.0d0 5.0d0)
                  (1.0d0 2.0d0 7.0d0 8.0d0)))
        (mat3 #2A((20.0d0 22.0d0 50.0d0 48.0d0)
                  (44.0d0 54.0d0 114.0d0 108.0d0)
                  (40.0d0 58.0d0 110.0d0 102.0d0)
                  (16.0d0 26.0d0 46.0d0 42.0d0))))
    (testing "should be true"
      (ok (equalp (mul mat1 mat2) mat3)))
    )
  (let ((mat1 #2A((1.0d0 2.0d0 3.0d0 4.0d0)
                  (2.0d0 4.0d0 4.0d0 2.0d0)
                  (8.0d0 6.0d0 4.0d0 1.0d0)
                  (0.0d0 0.0d0 0.0d0 1.0d0)))
        (tup1 #(1.0d0 2.0d0 3.0d0 1.0d0))
        )
    (testing "should be true"
      (ok (equalp (mul-tup mat1 tup1) #(18.0d0 24.0d0 33.0d0 1.0d0))))
    )
  (let ((mat1 #2A((1.0d0 2.0d0 3.0d0 4.0d0)
                  (2.0d0 4.0d0 4.0d0 2.0d0)
                  (8.0d0 6.0d0 4.0d0 1.0d0)
                  (0.0d0 0.0d0 0.0d0 1.0d0))))
    (testing "should be true"
      (ok (equalp (mul mat1 identity4) mat1)))
    )
  )

(deftest matrix-operations
  (let ((mat1 #2A((0.0d0 9.0d0 3.0d0 0.0d0)
                  (9.0d0 8.0d0 0.0d0 8.0d0)
                  (1.0d0 8.0d0 5.0d0 3.0d0)
                  (0.0d0 0.0d0 5.0d0 8.0d0)))
        (mat2 #2A((0.0d0 9.0d0 1.0d0 0.0d0)
                  (9.0d0 8.0d0 8.0d0 0.0d0)
                  (3.0d0 0.0d0 5.0d0 5.0d0)
                  (0.0d0 8.0d0 3.0d0 8.0d0))))
    (testing "should be true"
      (ok (equals? (transpose mat1) mat2)))
    (testing "transpose of identity is identity"
      (ok (equals? (transpose identity4) identity4)))
    )
  )

