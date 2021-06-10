(defpackage tuples
  (:use :cl)
  (:export :tuple :get-x :get-y :get-z :get-w :get-value
   :set-x :set-y :set-z :set-w
   :make-point :make-vector :point-p :vector-p :add :sub :mul :div :neg :magnitude :normalize)
  )

(in-package :tuples)

(defclass tuple ()
  ((data :initarg :value :initform (make-array 4 :initial-element 0.0) :accessor get-value)))

(defmethod get-x ((tup tuple)) (aref (slot-value tup 'data) 0))
(defmethod get-y ((tup tuple)) (aref (slot-value tup 'data) 1))
(defmethod get-z ((tup tuple)) (aref (slot-value tup 'data) 2))
(defmethod get-w ((tup tuple)) (aref (slot-value tup 'data) 3))
(defmethod set-x ((tup tuple) (value number)) (setf (aref (slot-value tup 'data) 0) value))
(defmethod set-y ((tup tuple) (value number)) (setf (aref (slot-value tup 'data) 1) value))
(defmethod set-z ((tup tuple) (value number)) (setf (aref (slot-value tup 'data) 2) value))
(defmethod set-w ((tup tuple) (value number)) (setf (aref (slot-value tup 'data) 3) value))

(defun make-point (x y z)
  (make-instance 'tuple :value (make-array 4 :initial-contents (list x y z 1.0))))

(defun make-vector (x y z)
  (make-instance 'tuple :value (make-array 4 :initial-contents (list x y z 0.0))))

(defun point-p (p)
  (and (typep p 'tuple) (equal (get-w p) 1.0)))

(defun vector-p (p)
  (and (typep p 'tuple) (equal (get-w p) 0.0)))

(defmethod add ((t1 tuple) (t2 tuple))
  (make-instance 'tuple :value (make-array 4 :initial-contents (list (+ (get-x t1) (get-x t2))
                                                                     (+ (get-y t1) (get-y t2))
                                                                     (+ (get-z t1) (get-z t2))
                                                                     (+ (get-w t1) (get-w t2))))))

(defmethod add ((t1 tuple) (t2 number))
  (make-instance 'tuple :value (make-array 4 :initial-contents (list (+ (get-x t1) t2)
                                                                     (+ (get-y t1) t2)
                                                                     (+ (get-z t1) t2)
                                                                     (+ (get-w t1) t2)))))

(defmethod add ((t1 number) (t2 tuple))
  (make-instance 'tuple :value (make-array 4 :initial-contents (list (+ t1 (get-x t2))
                                                                     (+ t1 (get-y t2))
                                                                     (+ t1 (get-z t2))
                                                                     (+ t1 (get-w t2))))))

(defmethod sub ((t1 tuple) (t2 tuple))
  (make-instance 'tuple :value (make-array 4 :initial-contents (list (- (get-x t1) (get-x t2))
                                                                     (- (get-y t1) (get-y t2))
                                                                     (- (get-z t1) (get-z t2))
                                                                     (- (get-w t1) (get-w t2))))))

(defmethod sub ((t1 tuple) (t2 number))
  (make-instance 'tuple :value (make-array 4 :initial-contents (list (- (get-x t1) t2)
                                                                     (- (get-y t1) t2)
                                                                     (- (get-z t1) t2)
                                                                     (- (get-w t1) t2)))))

(defmethod sub ((t1 number) (t2 tuple))
  (make-instance 'tuple :value (make-array 4 :initial-contents (list (- t1 (get-x t2))
                                                                     (- t1 (get-y t2))
                                                                     (- t1 (get-z t2))
                                                                     (- t1 (get-w t2))))))

(defmethod neg ((t1 tuple))
  (make-instance 'tuple :value (make-array 4 :initial-contents (list (- t1)
                                                                     (- t1)
                                                                     (- t1)
                                                                     (- t1)))))

(defmethod mul ((t1 tuple) (t2 number))
  (make-instance 'tuple :value (make-array 4 :initial-contents (list (* (get-x t1) t2)
                                                                     (* (get-y t1) t2)
                                                                     (* (get-z t1) t2)
                                                                     (* (get-w t1) t2)))))

(defmethod mul ((t1 number) (t2 tuple))
  (make-instance 'tuple :value (make-array 4 :initial-contents (list (* t1 (get-x t2))
                                                                     (* t1 (get-y t2))
                                                                     (* t1 (get-z t2))
                                                                     (* t1 (get-w t2))))))


(defmethod div ((t1 tuple) (t2 number))
  (make-instance 'tuple :value (make-array 4 :initial-contents (list (/ (get-x t1) t2)
                                                                     (/ (get-y t1) t2)
                                                                     (/ (get-z t1) t2)
                                                                     (/ (get-w t1) t2)))))

(defmethod magnitude ((t1 tuple))
  (sqrt (+ (* (get-x t1) (get-x t1))
           (* (get-y t1) (get-y t1))
           (* (get-z t1) (get-z t1))
           (* (get-w t1) (get-w t1))))
  
  )

(defmethod normalize ((t1 tuple))
  (let ((mag (magnitude t1)))
    (make-instance 'tuple :value (make-array 4 :initial-contents (list (/ (get-x t1) mag)
                                                                       (/ (get-y t1) mag)
                                                                       (/ (get-z t1) mag)
                                                                       (/ (get-w t1) mag))))
    )
  )


;; (defun dot (t1 t2)
;;   (+ (* (tuple-x t1) (tuple-x t2))
;;      (* (tuple-y t1) (tuple-y t2))
;;      (* (tuple-z t1) (tuple-z t2))
;;      (* (tuple-w t1) (tuple-w t2)))
;;   )


;; (defun cross (t1 t2)
;;   (make-vector (- (* (tuple-y t1) (tuple-z t2))
;;                   (* (tuple-z t1) (tuple-y t2)))
;;                (- (* (tuple-z t1) (tuple-x t2))
;;                   (* (tuple-x t1) (tuple-z t2)))
;;                (- (* (tuple-x t1) (tuple-y t2))
;;                   (* (tuple-y t1) (tuple-x t2)))
;;               )
;;   )

