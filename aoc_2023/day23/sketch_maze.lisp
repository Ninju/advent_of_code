(ql:quickload :sketch)

(defpackage :aoc_2023/sketch
  (:use :cl :sketch)
  (:export

   :get-relative-xy-in-dims

   :make-grid
   :grid/dimensions
   :grid/canvas-width
   :grid/canvas-height
   :grid/cell-width
   :grid/cell-height

   :grid/canvas-xy-relative-to-cell-at
   :grid/canvas-xy-of-cell-at

   :isosceles-triangle
   :rotated-isosceles-triangle

   :rect-relative

   :with-translate-in-grid
   ))

(in-package :aoc_2023/sketch)

(defclass grid/object (standard-object)
  ((dimensions :reader grid/dimensions :initarg :dimensions)
   (canvas-width :reader grid/canvas-width :initarg :canvas-width)
   (canvas-height :reader grid/canvas-height :initarg :canvas-height)
   (cell-width :reader grid/cell-width)
   (cell-height :reader grid/cell-height)))

(defmethod initialize-instance :after ((object grid/object) &key)
  (let ((unit-dims (grid/dimensions object))
        (canvas-width (grid/canvas-width object))
        (canvas-height (grid/canvas-height object)))
    (destructuring-bind (units-high units-wide) unit-dims
      (setf (slot-value object 'cell-width) (/ canvas-width units-wide))
      (setf (slot-value object 'cell-height) (/ canvas-height units-high)))))

(defun make-grid (dims canvas-width canvas-height)
  (make-instance 'grid/object :dimensions dims
                              :canvas-width canvas-width
                              :canvas-height canvas-height))

(defun get-relative-xy-in-dims (relative-pos dims)
  (destructuring-bind (h w) dims
    (case relative-pos
      (:center (list (/ w 2)
                     (/ h 2)))

      (:top-right    (list w 0))
      (:top-left     (list 0 0))
      (:bottom-right (list w h))
      (:bottom-left  (list 0 h))

      (t (list 0 0)))))

(defun grid/canvas-xy-relative-to-cell-at (grid relative-pos u-x u-y)
  (destructuring-bind (dx dy)
      (get-relative-xy-in-dims relative-pos
                               (list (grid/cell-height grid)
                                     (grid/cell-width grid)))
    (destructuring-bind (x y) (grid/canvas-xy-of-cell-at grid u-x u-y)
      (list (+ x dx)
            (+ y dy)))))

(defun grid/canvas-xy-of-cell-at (grid u-x u-y)
  (with-slots (cell-width cell-height) grid
    (list (* u-x cell-width)
          (* u-y cell-height))))

(defun rotated-isosceles-triangle (angle x y width height)
  (with-rotate (angle x (+ y (/ height 2)))
    (polygon x y
             (+ x (/ width 2)) (+ y height)
             (- x (/ width 2)) (+ y height))))

(defun isosceles-triangle (x y width height)
  (polygon x y
           (+ x (/ width 2)) (+ y height)
           (- x (/ width 2)) (+ y height)))

(defmacro with-translate-in-grid ((grid relative-pos cell-x cell-y) &body body)
  (let ((canvas-x (gensym))
        (canvas-y (gensym)))
    `(destructuring-bind (,canvas-x ,canvas-y)
         (grid/canvas-xy-relative-to-cell-at ,grid
                                             ,relative-pos
                                             ,cell-x
                                             ,cell-y)
       (sketch:with-translate (,canvas-x ,canvas-y)
         ,(cons 'progn body)))))

(defun rect-relative (relative-pos x y w h)
  (destructuring-bind (dx dy)
      (get-relative-xy-in-dims relative-pos (list h w))
  (rect (- x dx) (- y dy) w h)))

;;; Code that's specific to day 23
;; (defun draw-maze-directed (x y direction)
;;   (let ((canvas-x (* x *maze-cell-width*))
;;         (canvas-y (* y *maze-cell-height*))
;;         (tri-width (* 0.6 *maze-cell-width*))
;;         (tri-height (* 0.6 *maze-cell-height*)))
;;     (rect canvas-x canvas-y *maze-cell-width* *maze-cell-height*)
;;     (with-pen (make-pen :fill +yellow+ :weight 5)
;;       (rotated-isosceles-triangle (* 90 direction)
;;                                   (+ canvas-x (/ *maze-cell-width* 2))
;;                                   (+ canvas-y (/ (- *maze-cell-height* tri-height) 2))
;;                                   tri-width
;;                                   tri-height))))

;; (defun draw-maze (arr)
;;   (destructuring-bind (h w) (array-dimensions arr)
;;     (loop for j from 0 below h do
;;       (loop for i from 0 below w do
;;         (let ((cell (aref arr j i)))
;;           (cond
;;             ((char= #\# cell) (draw-maze-wall i j))
;;             ((char= #\S cell) (draw-maze-start-position i j))
;;             ((char= #\F cell) (draw-maze-finish-position i j))
;;             ((char= #\. cell) (draw-maze-empty-cell i j))
;;             ((char= #\< cell) (draw-maze-directed i j *dwest*))
;;             ((char= #\> cell) (draw-maze-directed i j *deast*))
;;             ((char= #\v cell) (draw-maze-directed i j *dsouth*))
;;             (t (draw-maze-empty-cell i j))))))))



;; (defparameter *maze-cell-width*  10)
;; (defparameter *maze-cell-height* 10)
;; (defparameter *maze-cell-pen* (make-pen :fill (rgb 0.1 0.1 0.1 0.9)))

;; (defparameter *dnorth*  0)
;; (defparameter *deast* 1)
;; (defparameter *dsouth* 2)
;; (defparameter *dwest* 3)

;; (defun draw-maze-wall (x y))
;;   ;; (with-pen (make-pen :fill +cyan+)
;;   ;;   (rect (* x *maze-cell-width*)
;;   ;;         (* y *maze-cell-height*)
;;   ;;         *maze-cell-width* *maze-cell-height*)))

;; (defun draw-maze-start-position (x y)
;;   (rect (* x *maze-cell-width*)
;;         (* y *maze-cell-height*)
;;         *maze-cell-width* *maze-cell-height*)
;;   (with-pen (make-pen :fill +yellow+)
;;     (circle (+ (* x *maze-cell-width*) (/ *maze-cell-width* 2))
;;             (+ (* y *maze-cell-height*) (/ *maze-cell-height* 2))
;;             (/ *maze-cell-height* 4))))


;; (defun draw-maze-finish-position (x y)
;;   (rect (* x *maze-cell-width*)
;;         (* y *maze-cell-height*)
;;         *maze-cell-width* *maze-cell-height*)
;;   (with-pen (make-pen :fill +black+)
;;     (circle (+ (* x *maze-cell-width*) (/ *maze-cell-width* 2))
;;             (+ (* y *maze-cell-height*) (/ *maze-cell-height* 2))
;;             (/ *maze-cell-height* 4))))

;; (defun draw-maze-empty-cell (x y)
;;   (rect (* x *maze-cell-width*)
;;         (* y *maze-cell-height*)
;;         *maze-cell-width* *maze-cell-height*))
