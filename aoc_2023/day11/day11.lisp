(in-package :aoc_2023)

(ql:quickload :split-sequence)
(ql:quickload :cl-ppcre)
(ql:quickload :alexandria)
(ql:quickload :arrows)

(use-package :arrows)

(defparameter *day-number* 11)

(defparameter *universe-expansion-size* 1000000)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-pathname (filename)
    (arrows:-> filename
               (merge-pathnames (format nil "day~d/" *day-number*))
               (merge-pathnames #p"/home/alex/src/workspace/advent_of_code/aoc_2023/"))))

(defparameter *example-input* (get-pathname "example.txt"))
(defparameter *input* (get-pathname "input.txt"))

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

(defparameter +galaxy-char+ #\#)
(defparameter +space-char+ #\.)

(defun galaxy-p (char) (char= char +galaxy-char+))
(defun space-p (char) (char= char +space-char+))

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

(defun empty-p (lst)
  (equal (car lst) '()))

(defun load-universe-from-file (filename) (lib:read-file-lines filename))

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

(defun find-empty-rows (lines)
  (remove nil
          (loop for y from 0 below (length lines)
                collect
                (let ((line (elt lines y)))
                  (if (every #'space-p line)
                      y)))))

(defun expand-universe (lines)
  (let* ((empty-columns (find-columns lines #'space-p))
         (expanded-rows (reduce (lambda (res line)
                                  (if (every #'space-p line)
                                      (nconc res (list line line))
                                      (nconc res (list line))))
                                lines
                                :initial-value '()))
         (min-row-length (min-width expanded-rows))
         (result NIL))

    (loop for line in expanded-rows
          collect
          (reduce #'nconc
                  (loop for idx from 0 below min-row-length
                        collect
                        (if (find idx empty-columns)
                            (cons (elt line idx)
                                  (cons +space-char+ result))
                            (cons (elt line idx)
                                  result)))))))

(defun convert-universe-to-2d-array (lines)
  (lib:lists->2d-array lines))

(defun build-map (filename)
  (-> filename
      (load-universe-from-file)
      (expand-universe)
      (convert-universe-to-2d-array)))

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

(defun find-galaxy-positions (map)
  (find-in-map map +galaxy-char+ :test #'char=))

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

(defun part1 (input)
  (let ((map (build-map input)))
    (lib:sum (find-minimum-snake-distances-between-galaxies map))))

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

(defun adjust-for-universe-expansion (n)
  (* *universe-expansion-size* n))

(defun part2 (input)
   (let* ((lines (load-universe-from-file input))
          (map (convert-universe-to-2d-array lines))
          (distances (find-minimum-snake-distances-between-galaxies map))
          (galaxy-pairs (pair-positions (find-galaxy-positions map)))
          (empty-columns (find-empty-columns lines))
          (empty-rows (find-empty-rows lines))
          (col-adjustment (mapcar (lambda (pair)
                                    (number-of-columns-pair-between empty-columns
                                                                    pair))
                                  galaxy-pairs))
          (row-adjustment (mapcar (lambda (pair)
                                    (number-of-rows-pair-between empty-rows
                                                                    pair))
                                  galaxy-pairs)))
     (+ (reduce #'+ col-adjustment :key #'adjust-for-universe-expansion)
        (reduce #'+ row-adjustment :key #'adjust-for-universe-expansion)
        (lib:sum distances)

        ;; Adjust for double counting the step
        (reduce #'+ col-adjustment :key (lambda (n) (* -1 n)))
        (reduce #'+ row-adjustment :key (lambda (n) (* -1 n))))))


(let ((*universe-expansion-size* 1000000))
  (part2 *input*))

 ; => 550358864332 (40 bits, #x8023F1D1CC)
