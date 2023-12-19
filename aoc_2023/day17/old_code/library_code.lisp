(in-package :aoc_2023)

(defparameter *map* NIL)

(defun make-pos (x y) (list x y))

(defun add-points (p1 p2)
  (destructuring-bind (x y) p1
    (destructuring-bind (dx dy) p2
      (make-pos (+ x dx)
                (+ y dy)))))

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

(defun get-map-xy (map x y) (aref map y x))
(defun get-map-pos (map pos) (get-map-xy map (car pos) (cadr pos)))

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

(defun find-matching-map (map predicate)
  (let ((results '()))
    (destructuring-bind (h w) (array-dimensions map)
      (loop for j from 0 below h
            do
               (loop for i from 0 below w
                     do
                        (if (funcall predicate (get-map-xy map i j))
                            (progn
                              (setf results (cons (make-pos i j) results))
                              results)))))
    results))

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
