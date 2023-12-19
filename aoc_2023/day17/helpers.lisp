(in-package :aoc_2023)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-inputs-pathname (filename)
    (-> filename
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
