(in-package :aoc_2023)

(ql:quickload :split-sequence)
(ql:quickload :cl-ppcre)

(defparameter example-input "/home/alex/src/workspace/advent_of_code/aoc_2023/day4/example.txt")
(defparameter input "/home/alex/src/workspace/advent_of_code/aoc_2023/day4/input.txt")
(defparameter *number-scanner* (ppcre:create-scanner "[0-9]+"))

(defun get-main-lines ()
  (lib:read-file-lines input))

(defun get-example-lines ()
  (lib:read-file-lines example-input))


(defparameter example-line "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19")

(defun extract-numbers-from-line (str)
  (mapcar #'parse-integer
          (ppcre:split "\\s+"
                       (string-trim " " str))))

(defun extract-winning-and-chosen-numbers (numbers-line)
  (destructuring-bind (winning-numbers chosen-numbers)
      (ppcre:split "\\|" numbers-line)
    (mapcar #'extract-numbers-from-line
            (list winning-numbers chosen-numbers))))

(defun extract-card-details (card-line)
  (ppcre:register-groups-bind (card-no numbers)
      ("Card\\s+([0-9]+):(\.+)"
       card-line)
    (cons card-no
          (extract-winning-and-chosen-numbers numbers))))

(defun winning-numbers-in-card (scratchcard)
  (destructuring-bind (card-no . (wins choices))
      scratchcard
    (lib:seq-intersection wins choices)))

(defun scratchcard-value (scratchcard)
  (let* ((winning (winning-numbers-in-card scratchcard))
         (no-matches (length winning)))
    (if (= no-matches 0)
        0
        (expt 2 (- no-matches 1)))))

(loop for line in (get-main-lines)
      sum
      (scratchcard-value (extract-card-details line)))
