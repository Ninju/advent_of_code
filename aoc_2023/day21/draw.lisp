(defpackage :array/draw
  (:use :cl :aoc_2023/points :arrows)
  (:export
   :make-array-painter
   :draw-points-in-2d-space
   :draw-array
   :draw-array-of-chars))

(in-package :array/draw)

(defun pad-dims (n dims)
  (mapcar (lambda (d) (+ n d)) dims))

(defun rectangle-dims (rect)
  (destructuring-bind ((lower-x lower-y) (upper-x upper-y)) rect
    (list (abs (- upper-x lower-x))
          (abs (- upper-y lower-y)))))

(defun rectangle-bounds-of-path (path)
  (let ((start-pos (car path)))
    (let ((upper-x (x-coord start-pos))
          (lower-x (x-coord start-pos))
          (upper-y (y-coord start-pos))
          (lower-y (y-coord start-pos)))

      (loop for p in path
            do
               (setf upper-x (max (x-coord p) upper-x))
               (setf lower-x (min (x-coord p) lower-x))
               (setf upper-y (max (y-coord p) upper-y))
               (setf lower-y (min (y-coord p) lower-y)))
      (list (make-pos lower-x lower-y)
            (make-pos upper-x upper-y)))))

(defun array-from-path (path &rest args)
  (let* ((bounding-rect (rectangle-bounds-of-path path))
         (dims (->> bounding-rect
                    (rectangle-dims)
                    (reverse)       ;; because rect is (x y) but array is (h w)
                    (pad-dims 1)))) ;; 0 indexing: (0 0) to (10 10) is 11 in xy
    (apply #'make-array dims args)))

(defun draw-points-in-2d-space (non-empty-char empty-char points)
  (let ((canvas (array-from-path points :initial-element empty-char)))
    (loop for pos in points
          do
             (setf (aref canvas
                         (y-coord pos)
                         (x-coord pos))
                   non-empty-char))
    canvas))

(defun make-array-painter (cell-painter-fn
                           &optional (row-painter-fn (lambda (stream &rest args)
                                                       (declare (ignore args))
                                                       (format stream "~%"))))

  (lambda (stream array)
    (destructuring-bind (h w) (array-dimensions array)
      (loop for j below h do
        (loop for i below w do
          (let ((cell (aref array j i)))
            (funcall cell-painter-fn stream (make-pos j i) cell)))
        (funcall row-painter-fn stream j)))))

(defun draw-array (stream array cell-fn)
  (funcall (make-array-painter (lambda (s pos cell)
                                 (format s "~A" (funcall cell-fn cell pos))))
           stream
           array))

(defvar *char-array-painter*
  (make-array-painter (lambda (stream pos cell)
                        (declare (ignore pos))
                        (format stream "~a" cell))))

(defun draw-array-of-chars (array)
  (funcall *char-array-painter* *standard-output* array))

#+nil
(defparameter *test-painter*
  (make-array-painter (lambda (stream pos cell)
                        (cond
                          ((eq cell T) (format stream "X"))
                          (t (format stream "."))))))

#+nil
(let ((test-array (make-array '(5 5) :initial-element NIL)))
  (setf (aref test-array 3 3) T)
  (draw-array t test-array (lambda (c pos) (cond ((eq c T) "X")
                                                 (t ".")))))
