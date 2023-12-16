(in-package :aoc_2023)

(ql:quickload :split-sequence)
(ql:quickload :cl-ppcre)
(ql:quickload :alexandria)
(ql:quickload :arrows)

(use-package :arrows)

(defparameter *day-number* 16)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-inputs-pathname (filename)
    (arrows:-> filename
               (merge-pathnames (format nil "day~d/inputs/" *day-number*))
               (merge-pathnames #p"/home/alex/src/workspace/advent_of_code/aoc_2023/"))))

(defparameter *input* (get-inputs-pathname "input.txt"))

(defun out-of-bounds-p (pos)
  (destructuring-bind (w h) (array-dimensions *map*)
    (destructuring-bind (x y) pos
      (or (< x 0)
          (>= x w)
          (< y 0)
          (>= y h)))))

(defparameter *north* '(0 -1))
(defparameter *south* '(0  1))
(defparameter *west*  '(-1  0))
(defparameter *east*  '(1 0))

(defparameter *default-directions* (list *north* *south* *west* *east*))

(defun not-implemented (&optional description &rest body)
  "Placeholder function for bits of code that aren't implemented yet"
  (error (format nil "Not implemented: ~a"
                 (if description description "no description given"))))

(defparameter +splitter-horizontal-char+ #\-)
(defparameter +splitter-vertical-char+ #\|)
(defparameter +mirror-upward-char+ #\/)
(defparameter +mirror-downward-char+ #\\)

(defparameter +splitter-horizontal-char+ #\-)
(defparameter +splitter-vertical-char+ #\|)
(defparameter +mirror-upward-char+ #\/)
(defparameter +mirror-downward-char+ #\\)

(defun light-collides-with (from-direction tile-char)
  (cond
    ((and (or (equal *north* from-direction)
              (equal *south* from-direction))
          (char= tile-char +splitter-horizontal-char+)) (list *west* *east*))

    ((and (or (equal *west* from-direction)
              (equal *east* from-direction))
          (char= tile-char +splitter-vertical-char+)) (list *north* *south*))

    ;; Upward mirror
    ((and (equal *north* from-direction)
          (char= tile-char +mirror-upward-char+)) (list *west*))

    ((and (equal *east* from-direction)
          (char= tile-char +mirror-upward-char+)) (list *south*))

    ((and (equal *south* from-direction)
          (char= tile-char +mirror-upward-char+)) (list *east*))

    ((and (equal *west* from-direction)
          (char= tile-char +mirror-upward-char+)) (list *north*))

    ;; Downward mirror
    ((and (equal *north* from-direction)
          (char= tile-char +mirror-downward-char+)) (list *east*))

    ((and (equal *east* from-direction)
          (char= tile-char +mirror-downward-char+)) (list *north*))

    ((and (equal *south* from-direction)
          (char= tile-char +mirror-downward-char+)) (list *west*))

    ((and (equal *west* from-direction)
          (char= tile-char +mirror-downward-char+)) (list *south*))))
