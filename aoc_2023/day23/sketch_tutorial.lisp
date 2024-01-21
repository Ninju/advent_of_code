(load "~/quicklisp/setup.lisp")
(ql:quickload :sketch)

(defpackage :tutorial
  (:use :cl :sketch))

(in-package :tutorial)

(defsketch tutorial ()
  (rect 100 100 200 200))

;; (ql:quickload :sketch-examples)
;; (make-instance 'sketch-examples:stars)
;; (make-instance 'sketch-examples:hello-world)
;; (make-instance 'sketch-examples:sinewave)
;; (make-instance 'sketch-examples:brownian)
;; (make-instance 'sketch-examples:life)

(defparameter *maze-cell-width*  100)
(defparameter *maze-cell-height* 100)
(defparameter *maze-cell-pen* (make-pen :fill (rgb 0.1 0.1 0.1 0.9)))

(defparameter *dnorth*  0)
(defparameter *deast* 1)
(defparameter *dsouth* 2)
(defparameter *dwest* 3)

(defun draw-maze-wall (x y)
  (with-pen (make-pen :fill +cyan+)
    (rect (* x *maze-cell-width*)
          (* y *maze-cell-height*)
          *maze-cell-width* *maze-cell-height*)))

(defun draw-maze-start-position (x y)
  (rect (* x *maze-cell-width*)
        (* y *maze-cell-height*)
        *maze-cell-width* *maze-cell-height*)
  (with-pen (make-pen :fill +yellow+)
    (circle (+ (* x *maze-cell-width*) (/ *maze-cell-width* 2))
            (+ (* y *maze-cell-height*) (/ *maze-cell-height* 2))
            (/ *maze-cell-height* 4))))


(defun draw-maze-finish-position (x y)
  (rect (* x *maze-cell-width*)
        (* y *maze-cell-height*)
        *maze-cell-width* *maze-cell-height*)
  (with-pen (make-pen :fill +black+)
    (circle (+ (* x *maze-cell-width*) (/ *maze-cell-width* 2))
            (+ (* y *maze-cell-height*) (/ *maze-cell-height* 2))
            (/ *maze-cell-height* 4))))

(defun draw-maze-empty-cell (x y)
  (rect (* x *maze-cell-width*)
        (* y *maze-cell-height*)
        *maze-cell-width* *maze-cell-height*))

(defun rotated-isosceles-triangle (angle x y width height)
  (with-rotate (angle x (+ y (/ height 2)))
    (polygon x y
             (+ x (/ width 2)) (+ y height)
             (- x (/ width 2)) (+ y height))))

(defun isosceles-triangle (x y width height)
  (polygon x y
           (+ x (/ width 2)) (+ y height)
           (- x (/ width 2)) (+ y height)))

(defun draw-directed-triangle-north (x y width height)
  (rotated-isosceles-triangle 0 x y width height))

(defun draw-directed-triangle-east (x y width height)
  (rotated-isosceles-triangle 90 x y width height))

(defun draw-directed-triangle-west (x y width height)
  (rotated-isosceles-triangle 270 x y width height))

(defun draw-directed-triangle-south (x y width height)
  (rotated-isosceles-triangle 180 x y width height))

(defun draw-maze-directed (x y direction)
  (let ((canvas-x (* x *maze-cell-width*))
        (canvas-y (* y *maze-cell-height*))
        (tri-width (* 0.6 *maze-cell-width*))
        (tri-height (* 0.6 *maze-cell-height*)))
    (rect canvas-x canvas-y *maze-cell-width* *maze-cell-height*)
    (with-pen (make-pen :fill (rgb 0.3 0.3 0.3 0.8) :weight 5)
      (rotated-isosceles-triangle (* 90 direction)
                                  (+ canvas-x (/ *maze-cell-width* 2))
                                  (+ canvas-y (/ (- *maze-cell-height* tri-height) 2))
                                  tri-width
                                  tri-height))))

(defun draw-maze (arr)
  (destructuring-bind (h w) (array-dimensions arr)
    (loop for j from 0 below h do
      (loop for i from 0 below w do
        (let ((cell (aref arr j i)))
          (cond
            ((char= #\# cell) (draw-maze-wall i j))
            ((char= #\S cell) (draw-maze-start-position i j))
            ((char= #\F cell) (draw-maze-finish-position i j))
            ((char= #\. cell) (draw-maze-empty-cell i j))
            ((char= #\< cell) (draw-maze-directed i j *dwest*))
            ((char= #\> cell) (draw-maze-directed i j *deast*))
            ((char= #\v cell) (draw-maze-directed i j *dsouth*))
            (t (draw-maze-empty-cell i j))))))))

(defsketch maze
    ((width 800)
     (height 800))

  (background +cyan+)

  (with-pen *maze-cell-pen*
    (with-translate (30 30)
      (draw-maze #2A((#\S #\. #\# #\#)
                     (#\# #\. #\> #\.)
                     (#\# #\. #\# #\.)
                     (#\# #\v #\# #\.)
                     (#\. #\. #\. #\.)
                     (#\# #\# #\# #\F))))
    ;; (draw-directed-triangle-north 100 100 50 50)
    (with-translate (0 700)
      (draw-maze-directed 1 0 *dnorth*)
      (draw-maze-directed 2 0 *deast*)
      (draw-maze-directed 3 0 *dsouth*)
      (draw-maze-directed 4 0 *dwest*))

    (with-translate (500 500)
      (draw-directed-triangle-north 200 150 50 50)
      (draw-directed-triangle-east 250 200 50 50)
      (draw-directed-triangle-south 200 250 50 50)
      (draw-directed-triangle-west 150 200 50 50))))
;; (draw-maze-wall 3 3)
;; (draw-maze-empty-cell 1 1)
;; (draw-maze-empty-cell 1 2)
;; (draw-maze-empty-cell 2 2)
;; (draw-maze-directed 2 2 *dnorth*)))


(make-instance 'maze)

(defparameter *colors* (loop for i below 16 collect (random-color)))

(defsketch random-color-test ((title "random-color") (width 400) (height 100))
  (dotimes (x 8)
    (dotimes (y 2)
      (with-pen (make-pen :fill (elt *colors* (+ x (* y 8))))
        (rect (* x 50) (* y 50) 50 50)))))

(make-instance 'random-color-test)

(defsketch hash-color-test ((title "hash-color") (width 400) (height 100))
  (dotimes (i 128)
    (with-pen (make-pen :fill (hash-color i))
      (rect (* i (/ 400 128)) 0 (/ 400 128) 100))))


(make-instance 'hash-color-test)

(make-instance 'tutorial)

(defsketch tutorial ()
  (background +yellow+))
(defsketch tutorial ()
  (bezier 0 400 100 100 300 100 400 400))

(defsketch tutorial ()
  (dotimes (i 4)
    (ngon (+ i 3) (+ 50 (* i 100)) 200 20 20 (* i 20))))
(defsketch tutorial ()
  (polygon 100 100 200 150 300 100 200 200))
(defsketch tutorial ()
  (polyline 100 100 200 150 300 100
            200 200 100 100))
(defsketch tutorial ()
  (line 0 0 400 400)
  (line 400 0 0 400))
(defsketch tutorial ()
  (dotimes (i 10)
    (rect 0 (* i 40) (* (+ i 1) 40) 40))
  (circle 300 100 50))
(defsketch tutorial ()
  (dotimes (i 10)
    (rect 0 (* i 40) (* (+ i 1) 40) 40)))
(defsketch tutorial ()
  (dotimes (i 10)
    (rect (* i 40) (* i 40) 40 40)))
