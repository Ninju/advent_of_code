(in-package :aoc_2023)

(ql:quickload :cl-ppcre)

(defparameter *example-input* "/home/alex/src/workspace/advent_of_code/aoc_2023/day6/example.txt")
(defparameter *input* "/home/alex/src/workspace/advent_of_code/aoc_2023/day6/input.txt")

(defun get-main-lines ()
  (lib:read-file-lines *input*))

(defun get-example-lines ()
  (lib:read-file-lines *example-input*))

(defun remove-non-numbers (str)
  (ppcre:regex-replace-all "[^0-9 ]" str ""))

(defun extract-time-and-distances (line)
  (parse-integer
   (apply #'concatenate 'string (ppcre:all-matches-as-strings "[0-9]" line))))

(defun button-hold-to-distance (hold-time race-time-limit)
  (if (or (= hold-time 0)
          (>= hold-time race-time-limit))
      0
      (let ((time-remaining (- race-time-limit hold-time)))
        (* hold-time time-remaining))))

(defun find-ways-to-win (winning-distance race-time-limit)
  (format t "Finding solution: ~d ~d~%" winning-distance race-time-limit)
  (loop for n from 1 below race-time-limit
        when (>
              (button-hold-to-distance n race-time-limit)
              winning-distance)
          collect n))

(defun count-ways-to-win (winning-distance race-time-limit)
  (format t "Finding solution: ~d ~d~%" winning-distance race-time-limit)
  (loop for n from 1 below race-time-limit
        when (>
              (button-hold-to-distance n race-time-limit)
              winning-distance)
          count 1))

(defun main (filename)
  (let ((times-and-distances
          (loop for line in (lib:read-file-lines filename)
                collect
                (extract-time-and-distances line))))
    (destructuring-bind (time distance) times-and-distances
      (count-ways-to-win distance time))))

(main *input*)
