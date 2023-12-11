(in-package :aoc_2023)

(ql:quickload :split-sequence)
(ql:quickload :cl-ppcre)
(ql:quickload :alexandria)
(ql:quickload :arrows)

(defparameter *day-number* 10)

;; is a vertical pipe connecting north and south.
;; - is a horizontal pipe connecting east and west.
;; L is a 90-degree bend connecting north and east.
;; J is a 90-degree bend connecting north and west.
;; 7 is a 90-degree bend connecting south and west.
;; F is a 90-degree bend connecting south and east.
;; . is ground; there is no pipe in this tile.
;; S is the starting position of the animal; there is a pipe on this tile, but your sketch doesn't show what shape the pipe has.

(defparameter *pipe-chars* "|-LJ7F")
(defparameter *animal-char* #\S)
(defparameter *ground-char* #\.)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-pathname (filename)
    (arrows:-> filename
               (merge-pathnames (format nil "day~d/inputs/" *day-number*))
               (merge-pathnames #p"/home/alex/src/workspace/advent_of_code/aoc_2023/"))))

(defparameter *example-1-input* (get-pathname "example_1.txt"))
(defparameter *example-2-input* (get-pathname "example_2.txt"))
(defparameter *example-1-solution* (get-pathname "example_1_solution.txt"))
(defparameter *example-2-solution* (get-pathname "example_2_solution.txt"))

(defparameter *input* (get-pathname "input.txt"))

(defun build-map (filename)
  (let* ((lines (lib:read-file-lines filename)))
    (lib:lists->2d-array lines)))
(defparameter *map* (build-map *example-2-input*))

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

;; Keep track of how many times a cell has been visited
(defparameter *visited* (make-hash-table
                         :test #'equal
                         :size (* 2 (apply #'* (array-dimensions *map*)))))

(defun not-implemented (&optional description)
  "Placeholder function for bits of code that aren't implemented yet"
  (error (format nil "Not implemented: ~a"
                 (if description description "no description given"))))

(defun ground-p (char) (char= char #\.))
(defun animal-p (char) (char= char #\S))
(defun pipe-p   (char) (find char *pipe-chars*))
(defun pipe-pos (pos)  (pipe-p (get-map-xy pos)))

;; POINTS
(defun make-pos (x y) (list x y))

(defun add-points (p1 p2)
  (destructuring-bind (x y) p1
    (destructuring-bind (dx dy) p2
      (make-pos (+ x dx)
                (+ y dy)))))

(defun direction-from (pos1 pos2)
  (make-pos (- (car pos2)
               (car pos1))
            (- (cadr pos2)
               (cadr pos1))))

;; MAP HELPERS
(defun get-map    (x y) (aref *map* y x))
(defun get-map-xy (pos) (aref *map* (cadr pos) (car pos)))

;; DIRECTIONS
(defun convert-pipe-to-directions (pipe-char)
  (case pipe-char
    (#\| (list *north* *south*))
    (#\- (list *east* *west*))
    (#\L (list *north* *east*))
    (#\J (list *north* *west*))
    (#\7 (list *south* *west*))
    (#\F (list *south* *east*))))

(defun reverse-direction (direction)
  (mapcar (lambda (n) (* -1 n)) direction))

(defun apply-directions (pos directions)
  (mapcar
   (lambda (d) (add-points pos d))
     directions))

(defun find-starting-position ()
  (destructuring-bind (y x) (array-dimensions *map*)
    (block outer
      (loop for j from 0 below y
            do
               (loop for i from 0 below x
                     do
                        ;; (format t "~&Current location and value '(~d, ~d) = ~a~%" i j (get-map i j))
                        (if (animal-p (get-map i j))
                            (return-from outer (make-pos i j))))))))

(defun connects-to-p (pos p)
  (let* ((pipe (get-map-xy p))
         (relative-direction (direction-from p pos))
         (directions (convert-pipe-to-directions pipe)))
    (if (or (ground-p pipe) (animal-p pipe))
        NIL
        (progn
 ;;          (format t "~2&Does pipe at p=~a connect to position pos=~a?
 ;; ~&Pipe at ~a = ~a
 ;; ~&The direction from the pipe at p=~a to pos=~a is ~a
 ;; ~&Pipe char points in directions: ~a"
 ;;                p pos
 ;;                p pipe
 ;;                p pos relative-direction
 ;;                directions)
        (let ((answer (not (equal NIL (find relative-direction directions :test #'equal)))))
          ;; (format t "~&The answer is: ~A" answer)
          answer)))))

(defun visited-p (pos)
    (multiple-value-bind (n exists) (gethash pos *visited*)
        exists))

(defun clear-visited! () (clrhash *visited*))

(defun mark-visited! (pos)
  (multiple-value-bind (n exists) (gethash pos *visited*)
      (if (not exists)
          (setf (gethash pos *visited*) 1)
          (incf (gethash pos *visited*)))))

(defun remove-visited (lst) (remove-if #'visited-p lst))

(defun last-element (lst)
  (car (last lst)))

(defun get-neighbours (pos)
  (remove-if #'out-of-bounds-p (apply-directions pos *default-directions*)))

;; GET-CONNECTED-NEIGHBOURS
(defun get-connected-neighbours (pos)
  "Get all neighbouring pipes connected to `pos`

   ASSUME: pos is never the position of ground
  "
  (let ((current-char (get-map-xy pos)))
    (if (animal-p current-char)
        (let ((neighbours (get-neighbours pos)))
          (remove-if-not
           (lambda (pipe-pos) (connects-to-p pos pipe-pos))
           (remove-if-not #'pipe-pos neighbours)))
        (let ((directions (convert-pipe-to-directions current-char)))
          ;; (format t "~3&From pos=~a with pipe=~a, directions=~a~%
          ;;   "
          ;;         pos current-char directions)
          (remove-if-not
           (lambda (neighbour)
             (or (animal-p (get-map-xy neighbour))
                 (connects-to-p pos neighbour)))
           (apply-directions pos (convert-pipe-to-directions current-char)))))))

(defun sort-points (p1 p2 &optional (predicate #'<))
  (destructuring-bind (x1 y1) p1
    (destructuring-bind (x2 y2) p2
      (if (= x1 x2)
          (funcall predicate y1 y2)
          (funcall predicate x1 x2)))))

(defun backtrack (current-position current-path)
  ;; (format t "~&- Backtracking from ~a and path is ~a"
  ;;         current-position current-path)
  (if (not (cdr current-path)) ;; there's nowhere left to go
      NIL
      (search-loop (car current-path) ;; search from previous position
                   (cdr current-path))))

(defun search-loop (current-position current-path)
  ;; (if (not (visited-p current-position))
  ;;     (format t "~&+ Searching ~a , path = ~a~%"
  ;;             current-position current-path))
  (mark-visited! current-position)

  (let ((neighbouring-connections (get-connected-neighbours current-position)))
    (if (not (= 2 (length neighbouring-connections)))
        (backtrack current-position current-path)
        (let* ((next-connections (remove-visited neighbouring-connections))
               (next-position (car next-connections)))
          (if (not next-position)
              ;; check to see if we've landed back at the starting position
              (progn
                ;; (format t "~&        Last element in path: ~a" (last-element current-path))
                ;; (format t "~&        IN neighbouring connections? ~a" (last-element current-path))
                (if (find (last-element current-path) neighbouring-connections :test #'equal)
                    (cons current-position current-path)
                    ;;  (format t "~&!!!!! Back at the start! Current path = ~a" (cons current-position current-path)))

                    ;; otherwise we've hit a deadend
                    (backtrack current-position current-path)))

              (search-loop next-position (cons current-position current-path)))))))

(defun find-loop ()
  (format t "~5&------ STARTING LOOP SEARCH -----~%")
  (let ((start-pos (find-starting-position)))
    (clear-visited!)
    (let ((result (search-loop start-pos '())))
      (format t "~2&______ END OF SEARCH ______~%")
      result)))

(defun empty-p (lst)
  (equal (car lst) '()))

(defun unenclosed-area-search (start-pos path visited-marker current-path)
  (format t "+ Searching ~a with current path as ~a" start-pos current-path)
  (setf (gethash start-pos visited-marker) T)
  (let* ((neighbours (get-neighbours start-pos))
         (non-path-neighbours (remove-if
                               (lambda (n) (find n path :test #'equal))
                               neighbours))
         (unvisited-neighbours (remove-if
                                (lambda (n) (gethash n visited-marker))
                                non-path-neighbours)))
    (if (empty-p unvisited-neighbours)
        (if (not path)
            (alexandria:hash-table-keys visited-marker)
            (progn
              (format t "- Backtracking to ~a" (car current-path))
              (unenclosed-area-search (car current-path)
                                    path
                                    visited-marker
                                    (cdr current-path))))
        (unenclosed-area-search (car neighbours)
                                path
                                visited-marker
                                (cons start-pos current-path)))))



(defun find-area-outside-path (path)
  (unenclosed-area-search '(0 0)
                          path
                          (make-hash-table :test #'equal)
                          '()))


