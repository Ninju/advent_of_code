(in-package :aoc_2023)

(ql:quickload :split-sequence)
(ql:quickload :cl-ppcre)

(defparameter example-input "/home/alex/src/workspace/advent_of_code/aoc_lang_2022/day4/example.txt")
(defparameter input "/home/alex/src/workspace/advent_of_code/aoc_lang_2022/day4/input.txt")
(defparameter *number-scanner* (ppcre:create-scanner "[0-9]+"))

(defun get-main-lines ()
  (lib:read-file-lines input))

(defun get-example-lines ()
  (lib:read-file-lines example-input))


(defparameter example-line "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")
