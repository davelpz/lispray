(defpackage tuples
  (:use :cl)
  (:export :get-x :get-y :get-z :get-w :set-x :set-y :set-z :set-w
           :make-tuple :make-point :make-vector :point-p :vector-p :add :sub :mul
   :div :neg :magnitude :normalize :dot :cross))

(in-package :tuples)

(defmacro get-x (tup) `(aref ,tup 0))
(defmacro get-y (tup) `(aref ,tup 1))
(defmacro get-z (tup) `(aref ,tup 2))
(defmacro get-w (tup) `(aref ,tup 3))
(defmacro set-x (tup val) `(setf (get-x ,tup) ,val))
(defmacro set-y (tup val) `(setf (get-y ,tup) ,val))
(defmacro set-z (tup val) `(setf (get-z ,tup) ,val))
(defmacro set-w (tup val) `(setf (get-w ,tup) ,val))

(defun make-tuple (x y z w)
  (declare (double-float x y z w))
  (make-array 4 :initial-contents (list x y z w) :element-type 'double-float)
  )

(defun make-point (x y z)
  (make-tuple x y z 1.0d0)
  )

(defun make-vector (x y z)
  (make-tuple x y z 0.0d0)
  )


(defun tuple-p (p)
  (and (= (array-rank p) 1)
       (= (array-dimension p 0) 4)))

(defun point-p (p)
  (and (tuple-p p) 
       (= (aref p 3) 1.0d0)))

(defun vector-p (p)
  (and (tuple-p p) 
       (= (aref p 3) 0.0d0)))

(defmethod add ((t1 simple-array) (t2 simple-array))
  (make-tuple (+ (get-x t1) (get-x t2))                                        
              (+ (get-y t1) (get-y t2))
              (+ (get-z t1) (get-z t2))
              (+ (get-w t1) (get-w t2))))
(defmethod add ((t1 simple-array) (t2 number))
  (make-tuple (+ (get-x t1) t2)                                        
              (+ (get-y t1) t2)
              (+ (get-z t1) t2)
              (+ (get-w t1) t2)))
(defmethod add ((t1 number) (t2 simple-array))
  (make-tuple (+ t1 (get-x t2))                                        
              (+ t1 (get-y t2))
              (+ t1 (get-z t2))
              (+ t1 (get-w t2))))

(defmethod sub ((t1 simple-array) (t2 simple-array))
  (make-tuple (- (get-x t1) (get-x t2))                                        
              (- (get-y t1) (get-y t2))
              (- (get-z t1) (get-z t2))
              (- (get-w t1) (get-w t2))))
(defmethod sub ((t1 simple-array) (t2 number))
  (make-tuple (- (get-x t1) t2)                                        
              (- (get-y t1) t2)
              (- (get-z t1) t2)
              (- (get-w t1) t2)))
(defmethod sub ((t1 number) (t2 simple-array))
  (make-tuple (- t1 (get-x t2))                                        
              (- t1 (get-y t2))
              (- t1 (get-z t2))
              (- t1 (get-w t2))))


(defmethod neg ((t1 simple-array))
  (make-tuple (- (get-x t1))                                        
              (- (get-y t1))
              (- (get-z t1))
              (- (get-w t1))))


(defmethod mul ((t1 number) (t2 simple-array))
  (make-tuple (* t1 (get-x t2))                                        
              (* t1 (get-y t2))
              (* t1 (get-z t2))
              (* t1 (get-w t2))))
(defmethod mul ((t1 simple-array) (t2 number))
  (make-tuple (* (get-x t1) t2)                                        
              (* (get-y t1) t2)
              (* (get-z t1) t2)
              (* (get-w t1) t2)))


(defmethod div ((t1 simple-array) (t2 number))
  (make-tuple (/ (get-x t1) t2)                                        
              (/ (get-y t1) t2)
              (/ (get-z t1) t2)
              (/ (get-w t1) t2)))

(defmethod magnitude ((t1 simple-array))
  (sqrt (+ (* (get-x t1) (get-x t1))
           (* (get-y t1) (get-y t1))
           (* (get-z t1) (get-z t1))
           (* (get-w t1) (get-w t1)))))

(defmethod normalize ((t1 simple-array))
  (let ((mag (magnitude t1)))
    (make-tuple (/ (get-x t1) mag)
                (/ (get-y t1) mag)
                (/ (get-z t1) mag)
                (/ (get-w t1) mag))))


(defmethod dot ((t1 simple-array) (t2 simple-array))
  (+ (* (get-x t1) (get-x t2))
     (* (get-y t1) (get-y t2))
     (* (get-z t1) (get-z t2))
     (* (get-w t1) (get-w t2))))


(defmethod cross ((t1 simple-array) (t2 simple-array))
  (make-vector (- (* (get-y t1) (get-z t2))
                  (* (get-z t1) (get-y t2)))
               (- (* (get-z t1) (get-x t2))
                  (* (get-x t1) (get-z t2)))
               (- (* (get-x t1) (get-y t2))
                  (* (get-y t1) (get-x t2)))))

