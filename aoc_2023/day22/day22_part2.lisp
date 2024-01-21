(in-package :aoc_2023)

(ql:quickload :arrows)
(ql:quickload :alexandria)
(ql:quickload :cl-ppcre)
(ql:quickload :group-by)

(use-package :arrows)

(declaim (optimize (debug 3)))

;; PROGRAM HELPERS

(defparameter *day-number* 22)

(defparameter *ground-z* 1)

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

(defstruct (brick (:constructor %make-brick)) id left-edge right-edge bounding-rect)

(defparameter *brick-id-cursor* 0)

(defun next-id ()
  (incf *brick-id-cursor*))

;; (defun next-id ()
;;   (prog1 (elt "ABCDEFGHIJKLMNOPQRSTUVWXYZ" *brick-id-cursor*) (incf *brick-id-cursor*)))

(defun make-brick (lhs rhs)
  (%make-brick :id (next-id)
               :left-edge lhs
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

(defun brick-volume (brick) (max-z-coord brick))
(defun brick-on-ground-p (brick)
  (or (= *ground-z* (z-coord (left-edge brick)))
      (= *ground-z* (z-coord (right-edge brick)))))
(defun min-z-coord (brick)
  (min (z-coord (left-edge brick))
       (z-coord (right-edge brick))))

(defun max-z-coord (brick)
  (max (z-coord (left-edge brick))
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

(defun move-brick-directly-to-z (brick ground-zero)
  (with-slots (left-edge right-edge) brick
    (let ((dz (+ ground-zero (* -1 (min-z-coord brick)))))
      (make-brick
       (add-points-3d (list 0 0 dz) left-edge)
       (add-points-3d (list 0 0 dz) right-edge)))))

(defun move-brick-directly-to-z! (brick ground-zero)
  (with-slots (left-edge right-edge) brick
    (let ((dz (+ ground-zero (* -1 (min-z-coord brick)))))
       (setf left-edge (add-points-3d (list 0 0 dz) left-edge))
       (setf right-edge (add-points-3d (list 0 0 dz) right-edge))
      brick)))

(defstruct (rectangle (:constructor %make-rect))
  lower-corner
  upper-corner
  width
  height)

(defun rectangles-overlap-p (r1 r2)
  (with-slots ((l1 lower-corner) (u1 upper-corner)) r1
    (with-slots ((l2 lower-corner) (u2 upper-corner)) r2
      (and (<= (max (x-coord l1)
                    (x-coord l2))
               (min (x-coord u1)
                    (x-coord u2)))

           (<= (max (y-coord l1)
                    (y-coord l2))
               (min (y-coord u1)
                    (y-coord u2)))))))

(defun bricks-would-collide-p (b1 b2)
  (with-slots ((r1 bounding-rect)) b1
    (with-slots ((r2 bounding-rect)) b2
      (rectangles-overlap-p r1 r2))))

(defun make-rect (lower upper)
  (if (not (and (<= (y-coord lower) (y-coord upper))
                (<= (x-coord lower) (x-coord upper))))
      (error "make-rect: Upper/Lower bounds potentially in wrong order")
      (%make-rect :lower-corner lower
                  :upper-corner upper
                  :width (abs (+ 1 (- (x-coord upper) (x-coord lower))))
                  :height (abs (+ 1 (- (y-coord upper) (y-coord lower)))))))

(defun point-within-rect-p (pos rect)
  (rectangles-overlap-p (make-rect pos pos)
                        rect))

(defun invert (pos dims)
  (destructuring-bind (h w) dims
    (declare (ignore w))
    (list (x-coord pos)
          (- h (y-coord pos)))))

(defun visualize-z (coord-fn bricks dimensions)
  "Plots the bricks on the Z axis (vertical) against a horizontal (COORD-FN)

    COORD-FN extracts the correct coord from the brick for plotting
             (function is therefore generalised to work on (x z) and (y z) plots
    BRICKS is all bricks in the simulation
    DIMENSIONS should be a tuple of: max horizontal coord and max Z value
  "
  (let ((arr (make-array dimensions :initial-element #\.)))
    (array/draw:draw-array
     *standard-output*
     arr
     (lambda (_cell pos)
       (declare (ignore _cell))

       (let ((brick-at-pos (find-if
                            (lambda (b)
                              (let* ((lhs (left-edge b))
                                     (rhs (right-edge b))
                                     (u-lhs (funcall coord-fn lhs))
                                     (u-rhs (funcall coord-fn rhs))
                                     (z-lhs (z-coord lhs))
                                     (z-rhs (z-coord rhs)))

                                (point-within-rect-p
                                 (invert pos dimensions)
                                 (make-rect (list u-lhs z-lhs)
                                            (list u-rhs z-rhs)))))
                            bricks)))

         (if brick-at-pos
             (with-slots (id) brick-at-pos id)
             "."))))))

(defun brick-does-fall-p (brick-id bricks-pulled-ids reverse-support-network)
  (let ((supporters (gethash brick-id reverse-support-network)))
    (every (lambda (supporter)
             (member supporter bricks-pulled-ids :test #'equal))
           supporters)))

(defun chain-reaction-count (bricks dict support-network reverse-support-network)
  (protect-against-infinite-loop!)

  (if (not bricks)
      0
      (let*
          ((grouped-and-sorted-by-z-bricks (-> (group-by:group-by bricks
                                                                  :value #'identity
                                                                  :key (lambda (b)
                                                                         (max-z-coord (gethash b dict))))
                                               (sort #'< :key #'car)))
           (min-z-bricks (cdr (car grouped-and-sorted-by-z-bricks)))
           (other-bricks (apply #'append
                                (mapcar #'cdr
                                        (cdr
                                         grouped-and-sorted-by-z-bricks))))
           (falling-bricks
             (-> (loop for brick in min-z-bricks
                       append
                       (let ((supports (gethash brick support-network)))
                         (remove-if-not
                          (lambda (supported-brick-id)
                            (brick-does-fall-p supported-brick-id
                                               min-z-bricks
                                               reverse-support-network))
                          supports)))
                 (remove-duplicates :test #'equal))))
        ;; (format t "~&DICT = ~A (~A)"
        ;;         dict
        ;;         (alexandria:hash-table-alist dict))
        (+ (length falling-bricks)
           (chain-reaction-count (append other-bricks falling-bricks)
                                 dict
                                 support-network
                                 reverse-support-network)))))


(defun part2 (filename)
  (let ((*brick-id-cursor* 0))
    (let ((bricks (sort (load-from-file filename) #'< :key #'min-z-coord))
          (placed-bricks '())

          ;; KEY => LIST of supported bricks (i.e. KEY supports bricks in LIST)
          (support-network (make-hash-table :size 150 :test #'equal))

          ;; KEY => LIST where KEY is supported by bricks in LIST)
          (reverse-support-network (make-hash-table :size 150 :test #'equal)))

      ;; (format t "~2&INIT: Plotting X and Z")
      ;; (visualize-z #'x-coord bricks '(10 3))

      ;; (format t "~2&INIT: Plotting Y and Z")
      ;; (visualize-z #'y-coord bricks '(10 3))

      (loop for brick in bricks do
        (setf (gethash (brick-id brick) support-network) NIL)
        (setf (gethash (brick-id brick) reverse-support-network) NIL)

        (let ((collisions (remove-if-not (lambda (placed-brick)
                                           (bricks-would-collide-p brick placed-brick))
                                         placed-bricks)))

          (if (not collisions)
              (push (move-brick-directly-to-z! brick *ground-z*)
                    placed-bricks)

              (progn
                (let* ((max-z (reduce #'max collisions :key #'max-z-coord))
                       (proper-collisions (remove-if-not (lambda (z) (= z max-z))
                                                         collisions
                                                         :key #'max-z-coord)
                                          ))
                  (loop for collision in proper-collisions do
                    (setf (gethash (brick-id collision) support-network)
                          (cons (brick-id brick) (gethash (brick-id collision)
                                                          support-network)))

                    (setf (gethash (brick-id brick) reverse-support-network)
                          (cons (brick-id collision) (gethash (brick-id brick)
                                                              reverse-support-network))))

                  (push (move-brick-directly-to-z! brick
                                                   (+ 1 max-z))
                        placed-bricks))))))

      ;; (format t "~2&DONE: Plotting X and Z")
      ;; (visualize-z #'x-coord placed-bricks '(7 3))

      ;; (format t "~2&DONE: Plotting Y and Z")
      ;; (visualize-z #'y-coord placed-bricks '(7 3))

      ;; (build-chain-reaction-count)

      (let ((bricks-dict (make-hash-table :size 150 :test #'equal)))
        (loop for brick in placed-bricks do
          (setf (gethash (brick-id brick) bricks-dict) brick))
        (list bricks-dict support-network reverse-support-network)))))
;; )

(defun stash
    ()
  (format t "~10&---- START:")
  (destructuring-bind (dict support-net reverse-net) (part2 "input.txt")
    (loop for key in (alexandria:hash-table-keys support-net)
          sum
          (chain-reaction-count (list key) dict support-net reverse-net)))
  )

#+nil
(stash)

; DONE: Plotting X and Z...
; .G.
; .G.
; FFF
; D.E
; CCC
; .A.

; DONE: Plotting Y and Z...
; .G.
; .G.
; .F.
; EEE
; B.C
; AAA
;  => (((#\G) (#\F #\G) (#\E #\F) (#\D #\F) (#\C #\E #\D) (#\B #\E #\D)
;   (#\A #\G #\F #\C #\B))
;
;  ((#\G #\A #\F) (#\F #\A #\D #\E) (#\E #\B #\C) (#\D #\B #\C) (#\C #\A)
;   (#\B #\A) (#\A)))

;; +nil
;; (load-from-file "example.txt")
