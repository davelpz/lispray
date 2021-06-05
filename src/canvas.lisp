(defpackage canvas
  (:use :cl :colors)
  (:export :canvas :make-canvas :canvas-width :canvas-height :canvas-buffer
   :canvas-p :init-canvas :write-pixel :pixel-at :canvas-to-ppm
           :canvas-to-file))

(in-package :canvas)

(defstruct canvas (width 0.0) (height 0.0) (buffer nil))

(defun init-canvas (w h)
  (make-canvas :width w :height h :buffer (make-array (list h w) :initial-element (colors:make-color :red 0 :green 0 :blue 0)))
  )

(defun write-pixel (c x y value)
  (setf (aref (canvas-buffer c) y x) value))

(defun pixel-at (c x y)
  (aref (canvas-buffer c) y x))

(defun color-clamp (clr)
  (round (* 255 (cond ((< clr 0) 0)
                      ((> clr 1) 1)
                      (t clr)
                      )))
  )

(defun canvas-to-ppm-row (c strm row)
  (let ((end (canvas-width c))
        (buffer (canvas-buffer c))
        )
    (do ((col 0 (+ col 1)))
        ((= col end) t)
      (let ((clr (aref buffer row col)))
        (format strm "~a ~a ~a "
                (color-clamp (colors:color-red clr))
                (color-clamp (colors:color-green clr))
                (color-clamp (colors:color-blue clr)))
        )
      )
    (format strm "~%")
    )
  )

(defun canvas-to-ppm (c strm)
  (format strm "P3~%")
  (format strm "~a ~a~%" (canvas-width c) (canvas-height c))
  (format strm "255~%")
  (let ((end (canvas-height c)))
    (do ((row 0 (+ row 1)))
        ((= row end) t)
        (canvas-to-ppm-row c strm row)
      )
    )
  )

(defun canvas-to-file (c filename)
  (with-open-file (stream (merge-pathnames (make-pathname :name filename))
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (canvas-to-ppm c stream)
    )
  )
