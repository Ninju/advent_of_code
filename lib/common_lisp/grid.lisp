(in-package :lib)

(ql:quickload :split-sequence)
(ql:quickload :cl-ppcre)

(defparameter example-input "/home/alex/src/workspace/advent_of_code/aoc_lang_2022/day3/example.txt")
(defparameter input "/home/alex/src/workspace/advent_of_code/aoc_lang_2022/day3/input.txt")

(defparameter *lines* (lib:read-file-lines example-input))
(defparameter *adjacent-vectors*
  '(( 1  0)
    ( 0  1)
    (-1  0)
    ( 0 -1)))

(defparameter *adjacent-vectors-with-diagonals*
  '(( 1  0)
    ( 0  1)
    (-1  0)
    ( 0 -1)

    ( 1  1)
    (-1 -1)
    ( 1 -1)
    (-1  1)))

;; ((0, 0) :rock)
;; ((1, 0) :rock)
;; ((2, 0) :rock)
;; ((3, 0) :rock)
;;
;; ((1, 0) :rock)
;; ((0, 1) :rock)
;; ((1, 1) :rock)
;; ((2, 1) :rock)
;; ((0, 1) :rock)

(format nil "~{~{~a~^ ~}~%~}"
(let ((a (make-array '(7 4))))




(defparameter rock-shapes-parser
  '("####")

  '(".#."
    "###"
    ".#.")

  '("..#"
    "..#"
    "###")

  '("#"
    "#"
    "#"
    "#")

  '("##"
    "##"))

(defun flatten-removing-nils (grid)
  (mapcan (lambda (row) (remove 'nil row)) grid))

(defun grid-adjacent-points-to (point &key include-diagonals)
  (let ((offsets (if include-diagonals
                     *adjacent-vectors-with-diagonals*
                     *adjacent-vectors*)))
    (destructuring-bind (x y) point
      (mapcar (lambda (p)
                (destructuring-bind (x1 y1) p
                  (create-2d-point (+ x1 x)
                                   (+ y1 y))))
              offsets))))

(defun grid-out-of-bounds-limiter (lower upper)
  (lambda (point)
    (destructuring-bind (lower-x lower-y) lower
      (destructuring-bind (upper-x upper-y) upper
        (destructuring-bind (x y) point
          (or (< x lower-x)
              (> x upper-x)
              (< y lower-y)
              (> y upper-y)))))))

(defun create-2d-point (i j) (list i j))
(defun get-2d-point-x (p) (car p))
(defun get-2d-point-y (p) (cdr p))

(defun objects-adj-to-point (grid x y) 'not-implemented)

(defun grid-at (grid x y) (elt (elt grid y) x))

(defun grid-map-with-indicies (grid fn)
  (loop for j from 0 below (length grid)
        collect
        (loop for i from 0 below (length (elt grid j))
              collect
              (funcall fn i j (grid-at grid i j)))))

(defun grid-map (grid fn)
  (grid-map-with-indicies grid (lambda (_i _j e) (funcall fn e))))

(defun grid-search (grid element &key (test #'equal) keep-nils)
  (let ((positions (grid-map-with-indicies grid
                          (lambda (i j e)
                                 (if (funcall test e element)
                                     (create-2d-point i j))))))
    (if keep-nils
        positions
        (flatten-removing-nils positions))))

(grid-search *lines* #\* :keep-nils t)
(grid-map (lib:read-file-lines input) (lambda (element) (equal element #\*)))
