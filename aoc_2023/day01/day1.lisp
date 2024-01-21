(in-package :aoc_2023)

(ql:quickload "cl-ppcre")

(defparameter file-input1 "/home/alex/src/workspace/advent_of_code/aoc_2023/day1/input_2023_day1.txt")
(defparameter test-input1 "/home/alex/src/workspace/advent_of_code/aoc_2023/day1/test.txt")

(defparameter digit-regex "(one|two|three|four|five|six|seven|eight|nine|[1-9])")

(defparameter digit-lookup
  '(("one" . "1")
    ("two" . "2")
    ("three" . "3")
    ("four" . "4")
    ("five" . "5")
    ("six" . "6")
    ("seven" . "7")
    ("eight" . "8")
    ("nine" . "9")

    ("1" . "1")
    ("2" . "2")
    ("3" . "3")
    ("4" . "4")
    ("5" . "5")
    ("6" . "6")
    ("7" . "7")
    ("8" . "8")
    ("9" . "9")))

(defun read-file-lines (filename)
  (uiop:read-file-lines filename))

(defun parse-file (filename)
  (read-file-lines filename))

(defun digits (str)
  "Note: will contain duplicated digits."
  (greedy-match-as-strings str))

(defun alist-str-lookup (key alist)
  (assoc key alist :test 'equal))

(defun greedy-match-as-strings (str)
  (remove nil (loop for idx from 0 to (length str)
    collect (ppcre:scan-to-strings digit-regex str :start idx))))

(defun parsed-digits (str)
  (mapcar (lambda (match)
            (cdr (alist-str-lookup match digit-lookup)))
          (digits str)))

(defun first-and-last (lst)
  (cons (first lst)
        (last lst)))

(defun concat-strings (lst)
  (format nil "~{~a~}" lst))

(defun sum (numbers)
  (reduce #'+ numbers))

(defun main (&optional (filename file-input1) )
  (let* ((inputs (parse-file filename))
         (results
           (mapcar #'parse-integer
                   (mapcar #'concat-strings
                           (mapcar #'first-and-last
                                   (mapcar #'parsed-digits inputs))))))

    (print
     (sum results))))

(main)
