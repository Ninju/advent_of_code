(in-package :aoc_2023)

(ql:quickload :split-sequence)
(ql:quickload :cl-ppcre)
(ql:quickload :alexandria)
(ql:quickload :arrows)

(use-package :arrows)

(defparameter *day-number* 14)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-inputs-pathname (filename)
    (arrows:-> filename
               (merge-pathnames (format nil "day~d/inputs/" *day-number*))
               (merge-pathnames #p"/home/alex/src/workspace/advent_of_code/aoc_2023/"))))

(defparameter *input* (get-inputs-pathname "input.txt"))

(defun out-of-bounds-p (pos)
  (destructuring-bind (w h) (array-dimensions *map*)
    (destructuring-bind (x y) pos
      (or (< x 0)
          (>= x w)
          (< y 0)
          (>= y h)))))

(defparameter *north* '(0 -1))
(defparameter *south* '(0  1))
(defparameter *west*  '(-1  0))
(defparameter *east*  '(1 0))

(defparameter *default-directions* (list *north* *south* *west* *east*))

(defun not-implemented (&optional description &rest body)
  "Placeholder function for bits of code that aren't implemented yet"
  (error (format nil "Not implemented: ~a"
                 (if description description "no description given"))))

(defparameter +unmoveable-rock-char+ #\#)
(defparameter +ground-char+ #\.)
(defparameter +sliding-rock-char+ #\O)

(defun unmoveable-rock-p (char) (char= char +unmoveable-rock-char+))
(defun sliding-rock-p (char) (char= char +sliding-rock-char+))
(defun ground-p (char) (char= char +ground-char+))

(defun make-pos (x y) (list x y))

(defun add-points (p1 p2)
  (destructuring-bind (x y) p1
    (destructuring-bind (dx dy) p2
      (make-pos (+ x dx)
                (+ y dy)))))

(defun get-map-xy (map x y) (aref map y x))

(defun direction-from (pos1 pos2)
  (make-pos (- (car pos2)
               (car pos1))
            (- (cadr pos2)
               (cadr pos1))))

(defun apply-directions (pos directions)
  (mapcar
   (lambda (d) (add-points pos d))
     directions))

(defun reverse-direction (direction)
  (mapcar (lambda (n) (* -1 n)) direction))

(defun last-element (lst)
  (car (last lst)))

(defun get-neighbours (pos)
  (remove-if #'out-of-bounds-p (apply-directions pos *default-directions*)))

(defun sort-points (p1 p2 &optional (predicate #'<))
  (destructuring-bind (x1 y1) p1
    (destructuring-bind (x2 y2) p2
      (if (= x1 x2)
          (funcall predicate y1 y2)
          (funcall predicate x1 x2)))))

(defun empty-p (lst) (not lst))

(defun min-width (lists)
  (reduce #'min lists :key #'length))

(defun find-columns (lines predicate)
  (remove nil
          (loop for x from 0 below (min-width lines)
                collect
                (let ((truthies (loop for y from 0 below (length lines)
                                      collect
                                      (let* ((row (elt lines y))
                                             (element (elt row x)))
                                        (funcall predicate element)))))
                  (if (every #'identity truthies) x)))))

(defun find-empty-columns (lines)
  (find-columns lines #'space-p))

(defun find-empty-rows (lines empty-cell-pred)
  (remove nil
          (loop for y from 0 below (length lines)
                collect
                (let ((line (elt lines y)))
                  (if (every empty-cell-pred line)
                      y)))))

(defun find-in-map (map item &key (test #'eql))
  (let ((results '()))
    (destructuring-bind (h w) (array-dimensions map)
      (loop for j from 0 below h
            do
               (loop for i from 0 below w
                     do
                        (if (funcall test item (get-map-xy map i j))
                            (progn
                              (setf results (cons (make-pos i j) results))
                              results)))))
    results))

(defun minimum-snake-distance (pos1 pos2)
  (destructuring-bind (x y) pos1
    (destructuring-bind (u v) pos2
      (+ (abs (- x u))
         (abs (- y v))))))

(defun pair-positions (positions)
  (mapcon (lambda (lst)
            (mapcar (lambda (cur)
                      (list (car lst) cur))
                    (cdr lst)))
          positions))

(defun find-minimum-snake-distances-between-pairs (positions)
  (mapcar (lambda (lst)
            (apply #'minimum-snake-distance lst))
          (pair-positions positions)))

(defun find-minimum-snake-distances-between-galaxies (map)
  (->> (find-galaxy-positions map)
       (reverse)
       (find-minimum-snake-distances-between-pairs)))

(defun between-col (x p1 p2)
  (destructuring-bind (x1 _y1) p1
    (destructuring-bind (x2 _y2) p2
      (or (and (< x1 x)
               (> x2 x))
          (and (< x2 x)
               (> x1 x))))))

(defun between-row (y p1 p2)
  (destructuring-bind (_x1 y1) p1
    (destructuring-bind (_x2 y2) p2
      (or (and (< y1 y)
               (> y2 y))
          (and (< y2 y)
               (> y1 y))))))

(defun number-of-intersects-between-positions (lst pair-of-positions predicate)
  (destructuring-bind (p1 p2) pair-of-positions
    (loop for e in lst
          count
          (funcall predicate e p1 p2))))

(defun number-of-columns-pair-between (columns pair)
  (number-of-intersects-between-positions columns pair #'between-col))

(defun number-of-rows-pair-between (rows pair)
  (number-of-intersects-between-positions rows pair #'between-row))

(let ((lines (lib:read-file-lines (get-inputs-pathname "input.txt"))))
  (let ((map-height (length lines))
        (column-limits (make-list (length (car lines)) :initial-element 0))
        (total-load 0))
    (loop for line in lines
          and row from 1
          do
             (loop for c across line
                   and col from 0
                   do
                      (if (unmoveable-rock-p c)
                          (setf (elt column-limits col) row)
                          (if (sliding-rock-p c)
                              (let ((column-limit (elt column-limits col)))
                                (incf total-load (- map-height column-limit))
                                (setf (elt column-limits col) (+ 1 column-limit)))))))
    total-load))
