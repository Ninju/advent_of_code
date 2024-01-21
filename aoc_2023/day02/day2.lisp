(in-package :aoc_2023)

(ql:quickload "cl-ppcre")

(defparameter filename-input "input.txt")
(defparameter filename-example "example.txt")

(defparameter example-result 8)

(defparameter limit-red 12)
(defparameter limit-green 13)
(defparameter limit-blue 14)

(setq input (lib:read-file-lines filename-example))

(defparameter regex-green "[0-9]+ green")
(defparameter regex-blue "[0-9]+ blue")
(defparameter regex-red "[0-9]+ red")
(defparameter regex-game "Game ([0-9]+)")

(defparameter regex-count "[0-9]+")

(defun get-counts (regex input-line)
  (mapcar (lambda (fltn)
            (parse-integer (car fltn)))
          (mapcar (lambda (str)
                    (ppcre:all-matches-as-strings regex-count str))
                  (ppcre:all-matches-as-strings regex input-line))))

(defun counts-breach-limit-p (counts limit)
  (some (lambda (n) (> n limit))
        counts))

(defun test-color-p (line regex limit)
  (let ((counts (get-counts regex line)))
    (counts-breach-limit-p counts limit)))

(defun possible-p (game)
  (not
   (or
    (test-color-p game regex-red limit-red)
    (test-color-p game regex-blue limit-blue)
    (test-color-p game regex-green limit-green))))

(defun parse-ints (ints)
  (mapcar #'parse-integer ints))

(defun get-game-number (game)
  (ppcre:scan-to-strings "[0-9]+" game))

(defun find-possible-games (games)
  (mapcar (lambda (possible-game)
            (ppcre:scan-to-strings regex-game possible-game))
          (remove-if-not #'possible-p input)))

(defun min-required-color (game regex limit)
  (reduce #'max (get-counts regex game)))

(defun min-required-colors (game)
  (let ((min-red (min-required-color game regex-red limit-red))
        (min-blue (min-required-color game regex-blue limit-blue))
        (min-green (min-required-color game regex-green limit-green)))
    (list min-red min-blue min-green)))

(defun power (lst)
  (reduce #'* lst))

(lib:sum (mapcar #'power
                 (mapcar #'min-required-colors (lib:read-file-lines filename-input))))
