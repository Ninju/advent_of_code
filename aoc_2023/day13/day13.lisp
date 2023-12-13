(in-package :aoc_2023)

(ql:quickload :split-sequence)
(ql:quickload :cl-permutation)
(ql:quickload :cl-ppcre)
(ql:quickload :alexandria)
(ql:quickload :arrows)

(use-package :arrows)

(defparameter *day-number* 13)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-inputs-pathname (filename)
    (arrows:-> filename
               (merge-pathnames (format nil "day~d/inputs/" *day-number*))
               (merge-pathnames #p"/home/alex/src/workspace/advent_of_code/aoc_2023/"))))

(defparameter *input* (get-inputs-pathname "input.txt"))

(defun note (&rest args) )
(defun not-implemented (&optional description &rest body)
  "Placeholder function for bits of code that aren't implemented yet"
  (error (format nil "Not implemented: ~a"
                 (if description description "no description given"))))

(defparameter +rock-char+ #\#)
(defparameter +ash-char+ #\.)
;; (defparameter *-regex* (ppcre:create-scanner "\\s+"))

(defun rock-p (c) (char= c +rock-char+))
(defun ash-p (c) (char= c +ash-char+))

(defun parse-rocks-file (filename) (lib:read-file-line-groups filename))

(defun get-column (map x)
  (let ((height (car (array-dimensions map))))
    (loop for y from 0 below height
          collect
          (aref map y x))))

(defun find-candidate-mirror-horizontal (map)
  (loop for p in (lib:consecutive map)
        and idx from 0
        when (destructuring-bind (a b) p (equal a b))
          collect idx))

(defun find-candidate-mirror-vertical (map)
  (let ((as-2d-array (lib:lists->2d-array map)))
    (destructuring-bind (h w) (array-dimensions as-2d-array)
      (loop for x from 0 below (- w 1)
            when (equal
                  (get-column as-2d-array x)
                  (get-column as-2d-array (+ 1 x)))
            collect
            x))))

(defun check-mirror-horizontal (map y)
  (block outer
    (loop for j from y downto 0
          do
             (let ((j2 (+ (- y j) 1 y)))
               (if (>= j2 (length map))
                   (return-from outer T)
                   (let ((lhs (elt map j))
                         (rhs (elt map j2)))
                     (if (not (equal lhs rhs))
                         (return-from outer NIL)))))
          )
    T))

(defun check-mirror-vertical (map x)
  (block outer
    (let ((as-2d-array (lib:lists->2d-array map))
          (width (reduce #'min map :key #'length)))
      (destructuring-bind (h w) (array-dimensions as-2d-array)
        (loop for i from x downto 0
              do
                 (let ((i2 (+ (- x i) 1 x)))
                   (if (>= i2 width)
                       (return-from outer T)
                       (let ((lhs (get-column as-2d-array i))
                             (rhs (get-column as-2d-array i2)))
                         (if (not (equal lhs rhs))
                             (return-from outer NIL))))))))
    T))

(defun find-mirrors (maps)
  (loop for map in maps
        collect
        (block continue
          (let ((candidate-horizontals (find-candidate-mirror-horizontal map)))
            (progn
              (loop for candidate in candidate-horizontals
                    do
                       (if (check-mirror-horizontal map candidate)
                           (return-from continue (list :horizontal candidate))))

              (let ((candidate-verticals (find-candidate-mirror-vertical map)))
                (loop for candidate in candidate-verticals
                      do
                         (if (check-mirror-vertical map candidate)
                             (return-from continue (list :vertical candidate))))))))))

(let ((n 0))
  (loop for mirror in (find-mirrors (parse-rocks-file (get-inputs-pathname "example.txt")))
        do
           (if (not mirror)
               (error "MISING MIRROR!")
               (setf n (+ n
                          (if (equal (car mirror) :horizontal)
                              (prog1 (* 100 (cadr mirror)) (print mirror) (print (cadr mirror)))
                              (cadr mirror))))
