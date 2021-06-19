(defpackage lispray/tests/matrix
  (:use :cl
   :matrix
        :rove))
(in-package :lispray/tests/matrix)

;; NOTE: To run this test file, execute `(asdf:test-system :tuples)' in your Lisp.
(defmacro df (val) 
  (let ((nv (coerce val 'double-float))) nv))

(defun mtrx (m)
  (let ((ans (make-matrix (matrix-dim m))))
    (dotimes (row (matrix-dim m))
      (dotimes (col (matrix-dim m))
        (set-xy ans row col (coerce (get-xy m row col) 'double-float))
        ))
    ans))

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
  (let ((mat1 #2A((1.0d0 5.0d0)
                  (-3.0d0 2.0d0)
                  )))
    (testing "should be true"
      (ok (= (determinant mat1) 17.0d0)))
    )
  (let ((mat1 #2A((1.0d0 5.0d0 0.0d0)
                  (-3.0d0 2.0d0 7.0d0)
                  (0.0d0 6.0d0 -3.0d0)
                  )))
    (testing "should be true"
      (ok (equals? (submatrix mat1 0 0) #2A ((2.0d0 7.0d0) (6.0d0 -3.0d0)))))
    (testing "should be true"
      (ok (equals? (submatrix mat1 0 2) #2A ((-3.0d0 2.0d0) (0.0d0 6.0d0)))))
    )
  (let ((mat1 #2A((-6.0d0 1.0d0 1.0d0 6.0d0)
                  (-8.0d0 5.0d0 8.0d0 6.0d0)
                  (-1.0d0 0.0d0 8.0d0 2.0d0)
                  (-7.0d0 1.0d0 -1.0d0 1.0d0)
                  ))
        (ans #2A ((-6.0d0 1.0d0 6.0d0)
                  (-8.0d0 8.0d0 6.0d0)
                  (-7.0d0 -1.0d0 1.0d0))))
    (testing "should be true"
      (ok (equals? (submatrix mat1 2 1) ans)))
    )
  (let ((mat1 #2A((3.0d0 5.0d0 0.0d0)
                  (2.0d0 -1.0d0 -7.0d0)
                  (6.0d0 -1.0d0 5.0d0)
                  )))
    (testing "should be true"
      (ok (= (minor mat1 1 0) 25.0d0)))
    (testing "should be true"
      (ok (= (minor mat1 0 0) -12.0d0)))
    (testing "should be true"
      (ok (= (cofactor mat1 0 0) -12.0d0)))
    (testing "should be true"
      (ok (= (minor mat1 1 0) 25.0d0)))
    (testing "should be true"
      (ok (= (cofactor mat1 1 0) -25.0d0)))
    )
  (let ((mat1 #2A((1.0d0 2.0d0 6.0d0)
                  (-5.0d0 8.0d0 -4.0d0)
                  (2.0d0 6.0d0 4.0d0)
                  )))
    (testing "should be true"
      (ok (= (cofactor mat1 0 0) 56.0d0)))
    (testing "should be true"
      (ok (= (cofactor mat1 0 1) 12.0d0)))
    (testing "should be true"
      (ok (= (cofactor mat1 0 2) -46.0d0)))
    (testing "should be true"
      (ok (= (determinant mat1) -196.0d0)))
    )
  (let ((mat1 #2A((-2.0d0 -8.0d0 3.0d0 5.0d0)
                  (-3.0d0 1.0d0 7.0d0 3.0d0)
                  (1.0d0 2.0d0 -9.0d0 6.0d0)
                  (-6.0d0 7.0d0 7.0d0 -9.0d0)
                  )))
    (testing "should be true"
      (ok (= (cofactor mat1 0 0) 690.0d0)))
    (testing "should be true"
      (ok (= (cofactor mat1 0 1) 447.0d0)))
    (testing "should be true"
      (ok (= (cofactor mat1 0 2) 210.0d0)))
    (testing "should be true"
      (ok (= (cofactor mat1 0 3) 51.0d0)))
    (testing "should be true"
      (ok (= (determinant mat1) -4071.0d0)))
    )
  )

(deftest matrix-inversion
  (let ((mat1 #2A((-5.0d0 2.0d0 6.0d0 -8.0d0)
                  (1.0d0 -5.0d0 1.0d0 8.0d0)
                  (7.0d0 7.0d0 -6.0d0 -7.0d0)
                  (1.0d0 -3.0d0 7.0d0 4.0d0)))
        (ans #2A((0.21804511278195488d0 0.45112781954887216d0
                                        0.24060150375939848d0 -0.045112781954887216d0)
                 (-0.8082706766917294d0 -1.4567669172932332d0
                                        -0.44360902255639095d0 0.5206766917293233d0)
                 (-0.07894736842105263d0 -0.2236842105263158d0
                                         -0.05263157894736842d0 0.19736842105263158d0)
                 (-0.5225563909774437d0 -0.8139097744360902d0
                                        -0.3007518796992481d0 0.30639097744360905d0))))
    (testing "should be true"
      (ok (= (determinant mat1) 532.0d0)))
    (testing "should be true"
      (ok (= (cofactor mat1 2 3) -160.0d0)))
    (testing "should be true"
      (ok (= (cofactor mat1 3 2) 105.0d0)))
    (testing "should be true"
      (ok (equals? (inverse mat1) ans)))
    (testing "should be true"
      (ok (= (get-xy (inverse mat1) 3 2) (/ -160.0d0 532.0d0))))
    (testing "should be true"
      (ok (= (get-xy (inverse mat1) 2 3) (/ 105.0d0 532.0d0))))
    )
  (let ((mat1 #2A ((8.0d0  -5.0d0  9.0d0  2.0d0) 
                   (7.0d0  5.0d0  6.0d0  1.0d0) 
                   (-6.0d0  0.0d0  9.0d0  6.0d0) 
                   (-3.0d0  0.0d0  -9.0d0  -4.0d0)))
        (ans #2A((-0.15384615384615385d0 -0.15384615384615385d0
                                         -0.28205128205128205d0 -0.5384615384615384d0)
                 (-0.07692307692307693d0 0.12307692307692308d0
                                         0.02564102564102564d0 0.03076923076923077d0)
                 (0.358974358974359d0 0.358974358974359d0 0.4358974358974359d0
                                      0.9230769230769231d0)
                 (-0.6923076923076923d0 -0.6923076923076923d0
                                        -0.7692307692307693d0 -1.9230769230769231d0))))
    (testing "should be true"
      (ok (equals? (inverse mat1) ans)))
    )
  (let ((mat1 #2A((9.0d0 3.0d0 0.0d0 9.0d0) 
                  (-5.0d0 -2.0d0 -6.0d0 -3.0d0) 
                  (-4.0d0 9.0d0 6.0d0 4.0d0)
                  (-7.0d0 6.0d0 6.0d0 2.0d0)))
        (ans #2A((-0.040740740740740744d0 -0.07777777777777778d0
                                          0.14444444444444443d0 -0.2222222222222222d0)
                 (-0.07777777777777778d0 0.03333333333333333d0
                                         0.36666666666666664d0 -0.3333333333333333d0)
                 (-0.029012345679012345d0 -0.14629629629629629d0
                                          -0.10925925925925926d0 0.12962962962962962d0)
                 (0.17777777777777778d0 0.06666666666666667d0
                                        -0.26666666666666666d0 0.3333333333333333d0))))
    (testing "should be true"
      (ok (equals? (inverse mat1) ans)))
    )
  (let* ((a (mtrx #2A((3 -9 7 3)
                      (3 -8 2 -9)
                      (-4 4 4 1)
                      (-6 5 -1 1))))
         (b (mtrx #2A((8 2 2 2)
                      (3 -1 7 0)
                      (7 0 5 4)
                      (6 -2 0 5))))
         (c (mul a b))
         (invb (inverse b))
         (cmulinvb (mul c invb)))
    (testing "should be true"
      (ok (equals? cmulinvb #2A((3.0d0 -9.0d0 7.0d0 3.000000000000001d0)
                                (2.999999999999999d0 -7.999999999999998d0
                                                     2.0000000000000036d0 -8.999999999999996d0)
                                (-3.9999999999999996d0 3.9999999999999996d0
                                                       3.9999999999999982d0 0.9999999999999993d0)
                                (-6.0d0 5.0d0 -1.0d0 0.9999999999999998d0))))))
  )

(deftest matrix-transform
  (let ((tr (transform 5.0d0 -3.0d0 2.0d0))
        (p (tuples:make-point -3.0d0 4.0d0 5.0d0)))
    (ok (equalp (mul-tup tr p) (tuples:make-point 2.0d0 1.0d0 7.0d0)))
    )
  (let* ((tr (transform 5.0d0 -3.0d0 2.0d0))
        (inv (inverse tr))
        (p (tuples:make-point -3.0d0 4.0d0 5.0d0)))
    (ok (equalp (mul-tup inv p) (tuples:make-point -8.0d0 7.0d0 3.0d0)))
    )
  (let ((tr (transform 5.0d0 -3.0d0 2.0d0))
        (v (tuples:make-vector -3.0d0 4.0d0 5.0d0)))
    (ok (equalp (mul-tup tr v) v))
    )
  )

(deftest matrix-scaling
  (let ((tr (scaling 2.0d0 3.0d0 4.0d0))
        (p (tuples:make-point -4.0d0 6.0d0 8.0d0)))
    (ok (equalp (mul-tup tr p) (tuples:make-point -8.0d0 18.0d0 32.0d0)))
    )
  (let ((tr (scaling 2.0d0 3.0d0 4.0d0))
        (p (tuples:make-vector -4.0d0 6.0d0 8.0d0)))
    (ok (equalp (mul-tup tr p) (tuples:make-vector -8.0d0 18.0d0 32.0d0)))
    )
  (let* ((tr (scaling 2.0d0 3.0d0 4.0d0))
         (inv (inverse tr))
         (p (tuples:make-vector -4.0d0 6.0d0 8.0d0)))
    (ok (equalp (mul-tup inv p) (tuples:make-vector -2.0d0 2.0d0 2.0d0)))
    )
  (let ((tr (scaling -1.0d0 1.0d0 1.0d0))
        (p (tuples:make-point 2.0d0 3.0d0 4.0d0)))
    (ok (equalp (mul-tup tr p) (tuples:make-point -2.0d0 3.0d0 4.0d0)))
    )
  )
