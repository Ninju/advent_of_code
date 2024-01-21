(in-package :cl-user)
(defvar *lla-configuration*
  '(:libraries ("/home/alex/src/workspace/advent_of_code/aoc_2023/libblas.so")))

(ql:quickload :arrows)
(ql:quickload :cl-ppcre)
(ql:quickload :lla)
(ql:quickload :3d-vectors)
(ql:quickload :fiveam)
(ql:quickload :generic-cl)

(defpackage :aoc_2023/day24
  (:use :cl :arrows :3d-vectors)
  (:shadowing-import-from :fiveam :is :def-suite :test :in-suite :run! :is-false)
  (:shadowing-import-from :generic-cl :=)
  (:local-nicknames (:v :3d-vectors)
                    (:re :cl-ppcre)))

(in-package :aoc_2023/day24)

(use-package :arrows)

(declaim (optimize (debug 3)))

;; PROGRAM HELPERS

(defparameter *day-number* 24)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-inputs-pathname (filename)
    (-> filename
        (merge-pathnames (format nil "day~d/inputs/" *day-number*))
        (merge-pathnames #p"/home/alex/src/workspace/advent_of_code/aoc_2023/"))))

(defparameter *iter-limit* 100000)

(defmacro protect-against-infinite-loop! ()
  `(progn
     (decf *iter-limit*)
     (if (< *iter-limit* 0)
         (progn (setf *iter-limit* 0)
                (error "Reached iteration limit!")))))

;; DATA STRUCTURES

(defun make-point (x y) (list x y))
(defun x-coord (point) (car point))
(defun y-coord (point) (cadr point))
(defun z-coord (point) (caddr point))

(defclass ray (standard-object)
  ((velocity :initarg :velocity :reader velocity)
   (initial-position :initarg :initial-position :reader initial-position)))

(defmethod print-object ((object ray) stream)
  (print-unreadable-object (object stream)
    (with-slots (initial-position velocity) object
      (format stream "~A ->~A" initial-position velocity))))
;; (format stream "(~A ~A) velocity->(~A ~A)"
;;         (vx initial-position)
;;         (vy initial-position)
;;         (vx velocity)
;;         (vy velocity)))))

(defun make-ray (initial-pos-velocity-pair)
  (destructuring-bind (initial-pos velocity) initial-pos-velocity-pair
    (make-instance 'ray
                   :velocity (apply #'vec velocity)
                   :initial-position (apply #'vec initial-pos))))

(defclass rectangle (standard-object)
  ((lower-bound :initarg :lower-bound :reader lower-bound)
   (upper-bound :initarg :upper-bound :reader upper-bound)))

(defmethod print-object ((object rectangle) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A to ~A" (lower-bound object) (upper-bound object))))

(defun make-rect (p1 p2)
  (make-instance 'rectangle :lower-bound (make-point (min (x-coord p1)
                                                          (x-coord p2))
                                                     (min (y-coord p1)
                                                          (y-coord p2)))
                            :upper-bound (make-point (max (x-coord p1)
                                                          (x-coord p2))
                                                     (max (y-coord p1)
                                                          (y-coord p2)))))

(defun lower-x (rect)
  (with-slots (lower-bound) rect
    (x-coord lower-bound)))

(defun upper-x (rect)
  (with-slots (upper-bound) rect
    (x-coord upper-bound)))

(defun lower-y (rect)
  (with-slots (lower-bound) rect
    (y-coord lower-bound)))

(defun upper-y (rect)
  (with-slots (upper-bound) rect
    (y-coord upper-bound)))

(defun point-within-rect (point rect)
  (not (or (< (x-coord point) (lower-x rect))
           (> (x-coord point) (upper-x rect))
           (< (y-coord point) (lower-y rect))
           (> (y-coord point) (upper-y rect)))))

(defun vec-within-rect (v rect)
  (point-within-rect (list (vx v) (vy v)) rect))

;; PROGRAM CONSTANTS

;; LOGIC

(defun cross-join (list)
  "CROSS-JOIN acts like CARTESIAN-PRODUCT without including elements with themselves

    e.g.
        (cross-join '(1))          ;; => NIL
        (cross-join '(1 2))        ;; => ((1 2))
        (cartesian-product '(1 2)) ;; => ((1 1) (1 2) (2 1) (2 2))

        (cross-join '(1 2 3))        ;; => ((1 2) (1 3) (2 3))
        (cartesian-product '(1 2 3)) ;; => ((1 1) (1 2) (1 3) (2 1) (2 2) (2 3) (3 1) (3 2) (3 3))
"
  (loop for idx from 0 below (length list)
        and x in list
        nconc
        (loop for y in (subseq list (+ idx
                                       1))
              collect (list x y))))

(defun same-sign-p (a b)
  (or (and (>= a 0)
           (>= b 0))
      (and (<= a 0)
           (<= b 0))))

(defun rays-parallel-p (ray-1 ray-2)
  (let* ((d1 (velocity ray-1))
         (d2 (velocity ray-2))
         (x1 (vx d1))
         (y1 (vy d1))
         (x2 (vx d2))
         (y2 (vy d2)))

      (and (same-sign-p x1 x2)
           (same-sign-p y1 y2)
           (= (/ y1 x1)
              (/ y2 x2)))))

;; TODO: Didn't find scalar multiply in :lla
(defun matrix-scalar-multiply (scalar matrix)
  (destructuring-bind (h w) (array-dimensions matrix)
    (let ((results (make-array (list h w))))
    (loop for j from 0 below h
          do
          (loop for i from 0 below w
                do
                (setf (aref results j i)
                      (* scalar (aref matrix j i)))))
      results)))

;; lla:solve fails on real input (see failing test case in test.lisp)
(defun solve-linear-equations (ma mb)
  (let* ((a (aref ma 0 0))
         (b (aref ma 0 1))
         (c (aref ma 1 0))
         (d (aref ma 1 1))

         (ad (* a d))
         (bc (* b c))

         ; lla:det returns 0 for determinant when dividing by 0
         (det (handler-case (/ 1 (- ad bc))
                (division-by-zero (c)
                  (declare (ignore c))
                  nil))))
    (when det
      (lla:mm
       (matrix-scalar-multiply
        det
        (make-array '(2 2) :initial-contents (list (list    d  (- b))
                                                   (list (- c)    a))))
       mb))))

(defun ray-find-intersection (ray-1 ray-2)
  (if (rays-parallel-p ray-1 ray-2)
      NIL
      (let ((d1 (velocity ray-1))
            (d2 (velocity ray-2))

            (i1 (initial-position ray-1))
            (i2 (initial-position ray-2)))

        (let ((A (make-array '(2 2)
                             :initial-contents (list (list (vx d1) (- (vx d2)))
                                                     (list (vy d1) (- (vy d2))))))
              (b (make-array '(2 1)
                             :initial-contents (list (list (- (vx i2) (vx i1)))
                                                     (list (- (vy i2) (vy i1)))))))
          (let ((solutions (solve-linear-equations A b)))
            (when solutions
              (let ((solution-a (aref solutions 0 0))
                    (solution-b (aref solutions 1 0)))
                ;; Rays can only look forwards in time, so negative values are not valid solutions
                (if (and (>= solution-a 0) (>= solution-b 0))
                    (v+ i2
                        (v* solution-b d2))))))))))

;; PARSING

(defun trim-whitespace (string)
  (string-trim '(#\Space) string))

(defun parse-integers (string &key (separator "\\s*,\\s*"))
  (->> string
       (trim-whitespace)
       (ppcre:split separator)
       (mapcar #'parse-integer)))

(defun parse-line-to-integers (line)
  (->> line
       (ppcre:split "@")
       (mapcar #'parse-integers)))

(defun drop-z-from-parsed-integers (parsed-integers)
  (mapcar #'butlast parsed-integers))

(defun parse-line (line)
  (->> line
       (parse-line-to-integers)
       (drop-z-from-parsed-integers)
       (make-ray)))

(defun load-data-from-file (filename)
  (->> filename
       (get-inputs-pathname)
       (lib:read-file-lines)
       (mapcar #'parse-line)))

;; END CODE

#+nil
(cartesian-product '(1 2 3 4 5))

(defun part1 (filename test-area)
  (->> (load-data-from-file filename)
       (cross-join)
       (mapcar #'(lambda (ray-pair)
                   (apply #'ray-find-intersection ray-pair)))
       (mapcar (lambda (p) (if p (vec-within-rect p test-area))))
       (count T)))

#+nil
(let ((test-area (make-rect '(7 7)
                            '(27 27)))
      (filename "example.txt"))
  (part1 filename test-area))

#+nil ;; LLA:LAPACK-FAILURE was signalled - det = infinity, could not fix (lla:det ..) says = 0 (?)
(lla:solve #2A((165.0 99.0) (-70.0 -42.0))
           #2A((7.65078e13) (4.0413965e12)))
#+nil
(let ((test-area (make-rect '(2e14 2e14)
                            '(4e14 4e14)))
      (filename "input.txt"))
  (part1 filename test-area))

#+nil
(let ((test-area (make-rect '(7 7)
                            '(27 27))))
  (->> (load-data-from-file "example.txt")
       (cross-join)
       (mapcar #'(lambda (ray-pair)
                   (apply #'ray-find-intersection ray-pair)))
       (mapcar (lambda (p) (if p (vec-within-rect p test-area))))
       (count T)))
