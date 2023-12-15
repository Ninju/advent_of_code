(in-package :aoc_2023)

(ql:quickload :split-sequence)
(ql:quickload :cl-ppcre)
(ql:quickload :alexandria)
(ql:quickload :arrows)

(use-package :arrows)

(defparameter *day-number* 15)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-inputs-pathname (filename)
    (arrows:-> filename
               (merge-pathnames (format nil "day~d/inputs/" *day-number*))
               (merge-pathnames #p"/home/alex/src/workspace/advent_of_code/aoc_2023/"))))

(defparameter *input* (get-inputs-pathname "input.txt"))


(defun step-current-value (current-value c)
  (-> (char-code c)
      (+ current-value)
      (* 17)
      (mod 256)))

(defun hash (string)
  (reduce #'step-current-value string :initial-value 0))

(hash "rn=1")

(let ((result 0))
  (loop for line in (lib:read-file-lines (get-inputs-pathname "input.txt"))
        do
           (reduce #'+ (ppcre:split "," )
                   :key #'hash
                   :initial-value 0)
