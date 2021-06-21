
(defpackage ray
  (:use :cl)
  (:export :ray :origin :direction :make-ray :ray-position))

(in-package :ray)

(defclass ray ()
  ((origin :accessor origin :initarg :origin :initform (tuples:make-point 0.0d0 0.0d0 0.0d0))
   (direction :accessor direction :initarg :direction :initform (tuples:make-vector 0.0d0 0.0d0 0.0d0))))

(defmacro make-ray (origin direction)
  `(make-instance 'ray :origin ,origin :direction ,direction))

(defun ray-position (r time)
  (tuples:add (origin r) (tuples:mul (direction r) time)))
