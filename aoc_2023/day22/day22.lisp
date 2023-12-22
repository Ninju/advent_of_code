(in-package :aoc_2023)

(ql:quickload :arrows)
(ql:quickload :alexandria)
(ql:quickload :cl-ppcre)

(use-package :arrows)

(declaim (optimize (debug 3)))

;; PROGRAM HELPERS

(defparameter *day-number* 21)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-inputs-pathname (filename)
    (-> filename
        (merge-pathnames (format nil "day~d/inputs/" *day-number*))
        (merge-pathnames #p"/home/alex/src/workspace/advent_of_code/aoc_2023/"))))

(defun not-implemented (&optional description &rest body)
  "Placeholder function for bits of code that aren't implemented yet"
  (declare (ignore body))
  (error (format nil "Not implemented: ~a"
                 (if description description "no description given"))))

(defparameter *iter-limit* 100000)

(defmacro protect-against-infinite-loop! ()
  `(progn
     (decf *iter-limit*)
     (if (< *iter-limit* 0)
         (progn (setf *iter-limit* 0)
                (error "Reached iteration limit!")))))

(defun make-point-3d (x y z) (list x y z))
(defun x-coord (pos) (car pos))
(defun y-coord (pos) (cadr pos))
(defun z-coord (pos) (caddr pos))

(defstruct (brick (:constructor %make-brick)) left-edge right-edge bounding-rect)

(defun make-brick (lhs rhs)
  (%make-brick :left-edge lhs
               :right-edge rhs
               :bounding-rect (apply #'make-rect
                                     (if (= (x-coord lhs)
                                            (x-coord rhs))
                                         (sort (list lhs rhs)
                                               #'<
                                               :key #'y-coord)
                                         (sort (list lhs rhs)
                                               #'<
                                               :key #'x-coord)))))

(defun left-edge (brick)
  (with-slots (left-edge) brick
    left-edge))

(defun right-edge (brick)
  (with-slots (right-edge) brick
    right-edge))

(defun brick-volume (brick) 0)
(defun brick-on-ground-p (brick)
  (or (= *ground-z* (z-coord (left-edge brick)))
      (= *ground-z* (z-coord (right-edge brick)))))
(defun min-z-coord (brick)
  (min (z-coord (left-edge brick))
       (z-coord (right-edge brick))))

(defun brick-x-coords (brick) (list (x-coord (left-edge brick))
                                    (x-coord (right-edge brick))))

(defun brick-y-coords (brick) (list (y-coord (left-edge brick))
                                    (y-coord (right-edge brick))))

(defun add-points-3d (p1 p2)
  (make-point-3d (+ (x-coord p1) (x-coord p2))
                 (+ (y-coord p1) (y-coord p2))
                 (+ (z-coord p1) (z-coord p2))))

(defun parse-coords (coords)
  (mapcar #'parse-integer (ppcre:split "," coords)))

(defun parse-line (line)
  (destructuring-bind (lhs rhs) (ppcre:split "~" line)
    (make-brick (parse-coords lhs) (parse-coords rhs))))

(defun load-from-file (filename)
  (->> (get-inputs-pathname filename)
       (lib:read-file-lines)
       (mapcar #'parse-line)))

#+nil
(load-from-file "example.txt")
