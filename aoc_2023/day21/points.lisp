(defpackage :aoc_2023/points
  (:use :cl)
  (:export
   :make-pos
   :x-coord
   :y-coord
   :add-points
   :direction-from
   :scale-point
   :reverse-direction))

(in-package :aoc_2023/points)

(defun make-pos (x y) (list x y))
(defun x-coord (pos) (destructuring-bind (x _y) pos (declare (ignore _y)) x))
(defun y-coord (pos) (destructuring-bind (_x y) pos (declare (ignore _x)) y))
(defun add-points (p1 p2)
  (list (+ (x-coord p1) (x-coord p2))
        (+ (y-coord p1) (y-coord p2))))

(defun direction-from (pos1 pos2)
  (make-pos (- (car pos2)
               (car pos1))
            (- (cadr pos2)
               (cadr pos1))))

(defun scale-point (p scalar)
  (make-pos (* (x-coord p) scalar)
            (* (y-coord p) scalar)))

(defun reverse-direction (direction)
  (scale-point direction -1))
