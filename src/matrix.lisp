;;
(defpackage matrix
  (:use :cl)
  (:export :make-matrix :set-xy :get-xy
   :matrix-p :equals? :matrix-dim :mul :mul-tup :identity4
   :transpose :determinant :submatrix :minor :cofactor
           :inverse :translation :scaling :rotation_x
           :rotation_y :rotation_z :shearing))

(in-package :matrix)

(defparameter identity4 #2A ((1.0d0 0.0d0 0.0d0 0.0d0)
                            (0.0d0 1.0d0 0.0d0 0.0d0)
                            (0.0d0 0.0d0 1.0d0 0.0d0)
                            (0.0d0 0.0d0 0.0d0 1.0d0)))
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

(defmacro matrix-dim (mat)
  `(array-dimension ,mat 0))

(defmacro equals? (m1 m2)
  `(equalp ,m1 ,m2))

(defun mul (m1 m2)
  (let ((result (make-matrix (matrix-dim m1))))
    (dotimes (row (matrix-dim m1))
      (dotimes (col (matrix-dim m1))
        (let ((val 0))
          (dotimes (i (matrix-dim m1))
            (setf val (+ val (* (get-xy m1 row i) (get-xy m2 i col))))
            )
          (set-xy result row col val)
          )
        )      
      )
    result)
  )

(defun mul-tup (m1 tup1)
  (let ((result (tuples:make-tuple 0.0d0 0.0d0 0.0d0 0.0d0)))
    (dotimes (row (matrix-dim m1))
      (let ((val 0))
        (dotimes (i (matrix-dim m1))
          (setf val (+ val (* (get-xy m1 row i) (aref tup1 i)))))
        (setf (aref result row) val)))
    result))

(defun transpose (m1)
  (let ((result (make-matrix (matrix-dim m1))))
    (dotimes (row (matrix-dim m1))
      (dotimes (col (matrix-dim m1))
        (set-xy result col row (get-xy m1 row col))
        )      
      )
    result)
  )

(defun determinant (m)
  (cond ((= (matrix-dim m) 2) (- (* (get-xy m 0 0) (get-xy m 1 1)) (* (get-xy m 0 1) (get-xy m 1 0))))
        (t (let ((det 0))
             (dotimes (col (matrix-dim m))
               (setf det (+ det (* (get-xy m 0 col) (cofactor m 0 col)))))
             det)))
  )

(defun submatrix (m row-to-delete col-to-delete)
  (let ((result (make-matrix (- (matrix-dim m) 1)))
        (r 0))
    (dotimes (row (matrix-dim m))
      (let ((c 0))
        (unless (= row row-to-delete)
            (dotimes (col (matrix-dim m))
              (unless (= col col-to-delete)
                ;;(format t "r=~a  c=~a    row=~a  col=~a~%" r c row col)
                (set-xy result r c (get-xy m row col))
                (incf c)))
            (incf r))))
    result)  
  )

(defun minor (m row col)
  (determinant (submatrix m row col))
  )

(defun cofactor (m row col)
  (if (oddp (+ row col))
      (- (minor m row col))
      (minor m row col))
  )

(defun inverse (m)
  (let ((det (determinant m)))
    (if (= det 0.0d0)
        (error "determinant can't be zero")
        (let ((result (make-matrix (matrix-dim m))))
          (dotimes (row (matrix-dim m))
            (dotimes (col (matrix-dim m))
              (set-xy result col row (/  (cofactor m row col) det))))
          result))))

(defun translation (x y z)
  (let ((ans (make-matrix 4)))
    (set-xy ans 0 0 1.0d0)
    (set-xy ans 1 1 1.0d0)
    (set-xy ans 2 2 1.0d0)
    (set-xy ans 3 3 1.0d0)
    (set-xy ans 0 3 x)
    (set-xy ans 1 3 y)
    (set-xy ans 2 3 z)
    ans))

(defun scaling (x y z)
  (let ((ans (make-matrix 4)))
    (set-xy ans 0 0 x)
    (set-xy ans 1 1 y)
    (set-xy ans 2 2 z)
    (set-xy ans 3 3 1.0d0)
    ans))

(defun rotation_x (ang)
  (let ((ans (make-matrix 4))
        (cosang (cos ang))
        (sinang (sin ang)))
    (set-xy ans 0 0 1.0d0)
    (set-xy ans 1 1 cosang)
    (set-xy ans 2 2 cosang)
    (set-xy ans 2 1 sinang)
    (set-xy ans 1 2 (- sinang))
    (set-xy ans 3 3 1.0d0)
    ans))

(defun rotation_y (ang)
  (let ((ans (make-matrix 4))
        (cosang (cos ang))
        (sinang (sin ang)))
    (set-xy ans 0 0 cosang)
    (set-xy ans 1 1 1.0d0)
    (set-xy ans 2 2 cosang)
    (set-xy ans 0 2 sinang)
    (set-xy ans 2 0 (- sinang))
    (set-xy ans 3 3 1.0d0)
    ans))

(defun rotation_z (ang)
  (let ((ans (make-matrix 4))
        (cosang (cos ang))
        (sinang (sin ang)))
    (set-xy ans 0 0 cosang)
    (set-xy ans 1 1 cosang)
    (set-xy ans 2 2 1.0d0)
    (set-xy ans 1 0 sinang)
    (set-xy ans 0 1 (- sinang))
    (set-xy ans 3 3 1.0d0)
    ans))

(defun shearing(xy xz yx yz zx zy)
  (let ((ans (make-matrix 4)))
    (set-xy ans 0 0 1.0d0)
    (set-xy ans 1 1 1.0d0)
    (set-xy ans 2 2 1.0d0)
    (set-xy ans 3 3 1.0d0)
    (set-xy ans 0 1 xy) 
    (set-xy ans 0 2 xz) 
    (set-xy ans 1 0 yx) 
    (set-xy ans 1 2 yz) 
    (set-xy ans 2 0 zx) 
    (set-xy ans 2 1 zy) 
    ans ))
