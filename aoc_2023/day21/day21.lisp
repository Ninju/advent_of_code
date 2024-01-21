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

(defparameter *iter-limit* 100000)

(defmacro protect-against-infinite-loop! ()
  `(progn
     (decf *iter-limit*)
     (if (< *iter-limit* 0)
         (progn (setf *iter-limit* 0)
                (error "Reached iteration limit!")))))

;; GRID SEARCHING

(defparameter *north* '(0 -1))
(defparameter *south* '(0  1))
(defparameter *west*  '(-1  0))
(defparameter *east*  '(1 0))

(defparameter *default-directions* (list *north*
                                         *east*
                                         *south*
                                         *west*))

(defparameter *map* NIL)
(defparameter *history* NIL)

(defun not-implemented (&optional description &rest body)
  "Placeholder function for bits of code that aren't implemented yet"
  (error (format nil "Not implemented: ~a"
                 (if description description "no description given"))))

(defun make-pos (x y) (list x y))

(defun get-map-xy (map x y) (aref map y x))
(defun get-map-pos (map pos) (get-map-xy map (x-coord pos) (y-coord pos)))

(defun direction-from (pos1 pos2)
  (make-pos (- (car pos2)
               (car pos1))
            (- (cadr pos2)
               (cadr pos1))))

(defun reverse-direction (direction)
  (mapcar (lambda (n) (* -1 n)) direction))

(defun last-element (lst)
  (car (last lst)))

(defun apply-directions (pos directions)
  (mapcar (lambda (d) (add-points pos d)) directions))

(defun get-neighbours (pos directions) (apply-directions pos directions))

(defun make-point (x y) (list x y))
(defun x-coord (pos) (destructuring-bind (x _y) pos (declare (ignore _y)) x))
(defun y-coord (pos) (destructuring-bind (_x y) pos (declare (ignore _x)) y))
(defun add-points (p1 p2)
  (list (+ (x-coord p1) (x-coord p2))
        (+ (y-coord p1) (y-coord p2))))

(defun scale-point (p scalar)
  (list (* (x-coord p) scalar)
        (* (y-coord p) scalar)))

(defun out-of-bounds-p (pos grid)
  (destructuring-bind (h w) (array-dimensions grid)
    (destructuring-bind (x y) pos
      (or (< x 0)
          (>= x w)
          (< y 0)
          (>= y h)))))

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

(defun breadth-first-traversal (grid
                                current-positions
                                get-neighbours-fn
                                mark-visited-fn
                                depth
                                max-depth)

  (when (> depth max-depth)
    (return-from breadth-first-traversal current-positions))

  (when (not current-positions)
    (return-from breadth-first-traversal NIL))

  (protect-against-infinite-loop!)

  (let ((next-positions '()))
    (loop for pos in current-positions
          do
             (funcall mark-visited-fn pos)

             (let ((neighbours (funcall get-neighbours-fn pos)))
               (when neighbours
                 (setf next-positions (append next-positions neighbours)))))

    (breadth-first-traversal grid
                             (remove-duplicates next-positions :test #'equal)
                             get-neighbours-fn
                             mark-visited-fn
                             (+ 1 depth)
                             max-depth)))


(defun get-starting-position (grid)
  (car (find-in-map grid #\S :test #'char=)))

(defun comp (&rest fs)
  (lambda (args)
    (reduce (lambda (acc fn)
              (funcall fn acc))
            (cdr (reverse fs))
            :initial-value (funcall (car (reverse fs)) args))))

(defun rock-p (c) (char= c #\#))
(defun garden-p (c) (char= c #\.))
(defun starting-position-p (c) (char= c #\S))

(defun at-pos-p (pos grid fn) (funcall fn (get-map-pos grid pos)))

(defun was-visited-p (pos visited)
  (aref visited (y-coord pos) (x-coord pos)))

(defun get-garden-plot-neighbours (pos grid visited)
  (->> (get-neighbours pos *default-directions*)
       (remove-if (lambda (n) (out-of-bounds-p n grid)))
       ;; (remove-if (lambda (n) (was-visited-p n visited)))
       (remove-if (lambda (n) (at-pos-p n grid #'rock-p)))))

(defun search-garden-plot (grid &optional (max-depth 64))
  (let ((starting-position (get-starting-position grid))
        (visited (make-array (array-dimensions grid) :initial-element NIL)))
    (when (not starting-position)
      (error "Error: No starting position found!"))

    (let ((final-visited-points
            (breadth-first-traversal
             grid
             (list starting-position)
             (lambda (pos) (get-garden-plot-neighbours pos grid visited))
             (lambda (p) (setf (aref visited (y-coord p) (x-coord p)) T))
             1
             max-depth)))

      (format t
              "~2&After exactly ~D steps, the following position was reached: ~%"
              max-depth)

      (array/draw:draw-array
       *standard-output*
       grid
       (lambda (cell pos)
         (cond ((member pos final-visited-points :test #'equal) "O")
               ((equal pos starting-position) "S")
               ((char= cell #\#) "#")
               (t "."))))

      final-visited-points)))

(defun part1 (filename &optional (steps 64))
  (let* ((grid (lib:lists->2d-array
                (lib:read-file-lines
                 (get-inputs-pathname filename)))))
    (->> (search-garden-plot grid steps)
         (length))))

#+nil
(part1 "input.txt" 64)

#+nil
(let* ((SLYNK-STICKERS:*BREAK-ON-STICKERS* (list :after))
       (grid (lib:lists->2d-array (lib:read-file-lines (get-inputs-pathname "example.txt"))))
       (visited (make-array (array-dimensions grid) :initial-element NIL)))
  (->> (search-garden-plot grid 1)))

       ;; (car)
       ;; (length)
       ;; (+ 1)))
       ;; ((lambda (g) (make-array (reduce #'* (array-dimensions g)) :displaced-to g)))
       ;; (count-if (lambda (e) (eq e T)))))

#+nil
  (->> (let ((visited (make-array '(5 5) :initial-element NIL)))
         (breadth-first-traversal
          grid
          (list '(2 2))
          (lambda (pos)
            (let ((*map* visited))
              (remove-if
               (lambda (p)
                 (aref visited
                       (cadr p)
                       (car p)))
               (remove-if #'out-of-bounds-p
                          (get-neighbours pos
                                          *default-directions*)))))
          (lambda (p)
            (setf (aref visited (cadr p) (car p)) T))
          1
          2)
         visited)
       (paint-array (lambda (e)
                      (cond
                        ((eq e T) #\X)
                        ((eq e NIL) #\.)
                        (t #\?)))))
