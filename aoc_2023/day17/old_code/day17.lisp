(in-package :aoc_2023)

(ql:quickload :split-sequence)
(ql:quickload :cl-ppcre)
(ql:quickload :alexandria)
(ql:quickload :arrows)
(use-package :arrows)

(defparameter *day-number* 17)
(defparameter *max-loop-iterations* 1000000)
(defparameter +empty-string-scanner+ (ppcre:create-scanner ""))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-inputs-pathname (filename)
    (arrows:-> filename
               (merge-pathnames (format nil "day~d/inputs/" *day-number*))
               (merge-pathnames #p"/home/alex/src/workspace/advent_of_code/aoc_2023/"))))

(defparameter *input* (get-inputs-pathname "input.txt"))

(defun parse-line (line)
  (mapcar #'parse-integer (ppcre:split +empty-string-scanner+
                                       line)))

(defun build-map (filename)
  (lib:lists->2d-array
   (mapcar #'parse-line
           (lib:read-file-lines (get-inputs-pathname filename)))))
