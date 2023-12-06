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
  (lib:read-file-lines *input*))

(defun get-example-lines ()
  (lib:read-file-lines *example-input*))

(defun main (&optional (filename *example-input*))
  (let ((*lines* lib:read-file-lines filename))
    (loop for line in *lines*
          collect

          (mapcar #'lib:extract-numbers-from-line (get-example-lines))
          line)))

(main)

(defun remove-non-numbers (str)
  (ppcre:regex-replace-all "[^0-9 ]" str ""))

(defun extract-time-and-distances (line)
  (lib:extract-numbers-from-line
   (remove-non-numbers line)))

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
  (length (find-ways-to-win winning-distance race-time-limit)))

(let ((times-and-distances
        (loop for line in (get-example-lines)
              collect
              (extract-time-and-distances line))))
  (reduce #'*
          (apply #'mapcar
                 (lambda (time distance)
                   (format t "Time: ~d, distance: ~d~%" time distance)
                   (count-ways-to-win distance time))
                 times-and-distances)))

(find-ways-to-win 9 7)
