(in-package :aoc_2023)

(ql:quickload :split-sequence)
(ql:quickload :cl-ppcre)
(ql:quickload :alexandria)

;; (ql:quickload :lparallel)
;; (defparameter *number-of-workers* 10)
;; (setf lparallel:*kernel* (lparallel:make-kernel *number-of-workers*))

(defparameter *example-input* "/home/alex/src/workspace/advent_of_code/aoc_2023/day6/example.txt")
(defparameter *input* "/home/alex/src/workspace/advent_of_code/aoc_2023/day6/input.txt")

(defun get-main-lines ()
  (lib:read-file-lines input))

(defun get-example-lines ()
  (lib:read-file-lines example-input))

(defun main (&optional (filename *example-input*))
  (let ((*lines* lib:read-file-lines filename))
    (loop for line in *lines*
          collect
          line)))
