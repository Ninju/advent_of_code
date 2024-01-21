(ql:quickload :cl-ppcre)
(ql:quickload :3d-vectors)
(ql:quickload :array-operations)
(ql:quickload :generic-cl)

;; (ql:quickload :select)
;; (defun column (col arr)
;;   (select:select arr t col))
;; (ql:quickload :let-plus)

;; (ql:quickload :metabang-bind)

(defpackage :aoc_2023/day23
  (:use :cl :arrows :3d-vectors)
  (:local-nicknames (:re :cl-ppcre))
  (:import-from :aoc_2023/sketch
   :rotated-isosceles-triangle
   :make-grid
   :grid/canvas-xy-relative-to-cell-at
   :grid/canvas-xy-of-cell-at
   :grid/dimensions
   :grid/canvas-width
   :grid/canvas-height
   :grid/cell-width
   :grid/cell-height
   :get-relative-xy-in-dims
   :rect-relative
   :with-translate-in-grid)
  (:import-from :sketch
                :defsketch
                :rect
                :circle
                :polygon
                :rgb)
  (:shadowing-import-from :generic-cl :=))

(in-package :aoc_2023/day23)

(declaim (optimize (debug 3)))

;; PROGRAM HELPERS

(defparameter *day-number* 23)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-inputs-pathname (filename)
    (-> filename
        (merge-pathnames (format nil "day~d/inputs/" *day-number*))
        (merge-pathnames #p"/home/alex/src/workspace/advent_of_code/aoc_2023/"))))

(defparameter *iter-limit* 2000)

(defmacro protect-against-infinite-loop! ()
  `(progn
     (decf *iter-limit*)
     (if (< *iter-limit* 0)
         (progn (setf *iter-limit* 0)
                (error "Reached iteration limit!")))))

;; GRID SEARCHING

(defparameter *north* (vec2 0 -1))
(defparameter *south* (vec2 0  1))
(defparameter *west*  (vec2 -1  0))
(defparameter *east*  (vec2 1 0))

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

(defun make-pos (x y) (vec2 x y))
(defun get-map-xy (map x y) (aref map y x))
(defun get-map-pos (map pos) (get-map-xy map (round (vx pos)) (round (vy pos))))
(defun get-neighbours (pos directions)
  (mapcar (lambda (d) (v+ pos d)) directions))

(defstruct (queue (:constructor %make-queue)) contents)

(defun make-queue (&rest initial-contents)
  (%make-queue :contents initial-contents))

(defun queue/push (queue &rest elements)
  (with-slots (contents) queue
    (setf contents (append contents elements))))

(defun queue/push-each (queue elements)
  (apply #'queue/push queue elements))

(defun queue/pop (queue)
  (with-slots (contents) queue
    (prog1 (car contents)
      (setf contents (cdr contents)))))

(defun queue/empty (queue)
  (with-slots (contents) queue
    (not contents)))

(defun list-from-queue (queue)
  (with-slots (contents) queue
    contents))

(defun last-element (lst)
  (car (last lst)))

(defun out-of-bounds-p (pos grid)
  (destructuring-bind (h w) (array-dimensions grid)
    (let ((x (vx pos))
          (y (vy pos)))
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

(defun get-map-neighbours (grid pos)
  (->>
   (let ((cur (get-map-pos grid pos)))
     (cond
       ((char= cur #\#) NIL)
       ((char= cur #\.) (get-neighbours pos *default-directions*))
       ((char= cur #\>) (get-neighbours pos (list *east*)))
       ((char= cur #\<) (get-neighbours pos (list *west*)))
       ((char= cur #\v) (get-neighbours pos (list *south*)))
       (t (error "Unexpected map character '~a' at pos ~d" cur pos))))
   (remove-if (lambda (pos) (out-of-bounds-p pos grid)))
   (remove-if (lambda (pos) (char= #\# (get-map-pos grid pos))))))

(defun hash-table-size-for-grid (2d-array)
  (* 2 (reduce #'* (array-dimensions 2d-array))))

(defun get-distance (pos distances)
  (multiple-value-bind (v exists) (gethash pos distances)
    (if (not exists)
        (prog1 0 (setf (gethash pos distances) 0))
        v)))

(defun set-distance (pos distances new-distance)
  (setf (gethash pos distances) new-distance))

(defun row (row arr)
  (aops:sub arr row))

(defun find-starting-position-grid (grid)
  (let ((pos (position #\. (row 0 grid))))
    (make-pos pos 0)))

(defun incf-distance (pos distances neighbour-distance)
  (let ((cur-distance (get-distance pos distances)))
      (when (<= cur-distance neighbour-distance)
        (set-distance pos distances (+ 1 neighbour-distance)))))

(defun choose-unvisited-with-distance (sort-fn unvisited distances)
  (values-list
   (macrolet ((distance-of (pos) `(gethash ,pos distances)))
     (reduce
      (lambda (acc pos)
        (destructuring-bind (cur rest) acc
          (if (funcall sort-fn (distance-of pos) (distance-of cur))
              (list pos (cons cur rest))
              (list cur (cons pos rest)))))
      (rest unvisited)
      :initial-value (list (first unvisited) '())))))

(defclass IDEA-longest-path-breadth-first-explorer (standard-object)
  ((maze :reader maze)
   (unvisited :initform (make-queue) :reader unvisited)
   (visited :reader visited)
   (distances :reader distances)))

(defclass IDEA-longest-tentative-distance-first-explorer (standard-object)
  ())

(defgeneric search-path (object maze starting-point
                         &optional target
                         &key listener commander))

(defmethod search-path ((explorer IDEA-longest-tentative-distance-first-explorer)
                        maze
                        starting-point
                        &optional target
                        &key (listener (lambda (event data))) (commander (lambda (event data))))

  (let ((unvisited NIL)
        (visited (make-hash-table :size (hash-table-size-for-grid maze)
                                  :test #'equalp))
        (distances (make-hash-table :size (hash-table-size-for-grid maze)
                                    :test #'equalp)))

        (setf (gethash starting-point distances) 0)

    (flet ((get-distance (pos) (or (gethash pos distances) 0))
           (set-visited! (pos) (setf (gethash pos visited) T))
           (visitedp (pos) (gethash pos visited)))

      (labels ((search-from (current-position)
                 (protect-against-infinite-loop!)

                 (cond
                   ((not current-position) "DONE")
                   ((and target (equalp current-position target)) "DONE")
                   (t

                    (funcall listener :visiting
                             (list :position current-position
                                   :distances distances
                                   :visited visited))

                    (let ((current-distance (get-distance current-position))
                          (neighbours (remove-if
                                       #'visitedp
                                       (get-map-neighbours maze
                                                           current-position))))

                      (loop for neighbour in neighbours do
                        (let ((neighbour-distance (get-distance neighbour)))
                          (setf (gethash neighbour distances)
                                (max (+ 1 current-distance)
                                     neighbour-distance))))

                      (set-visited! current-position)

                      (multiple-value-bind (next-unvisited rest-unvisited)
                          (choose-unvisited-with-distance #'>
                                                          (append neighbours unvisited)
                                                          distances)

                        (setf unvisited rest-unvisited)

                        (search-from next-unvisited)))))))
        (search-from starting-point)))))

(defmethod search-path ((explorer IDEA-longest-path-breadth-first-explorer)
                        maze
                        starting-point
                        &optional target
                        &key (listener (lambda (event data))) (commander (lambda (event data))))

  (let ((unvisited (make-queue))
        (visited (make-hash-table :size (hash-table-size-for-grid maze)
                                  :test #'equalp))
        (distances (make-hash-table :size (hash-table-size-for-grid maze)
                                    :test #'equalp)))

        (setf (gethash starting-point distances) 0)

  (flet ((get-distance (pos) (or (gethash pos distances) 0))
         (set-visited! (pos) (setf (gethash pos visited) T))
         (visitedp (pos) (gethash pos visited)))

    (labels ((search-from (current-position)
               (protect-against-infinite-loop!)

               (cond
                 ((not current-position) "DONE")
                 ((and target (equalp current-position target)) "DONE")
                 (t

                  (funcall listener :visiting
                           (list :position current-position
                                 :distances distances
                                 :visited visited))

                  (let ((current-distance (get-distance current-position))
                        (neighbours (remove-if
                                     #'visitedp
                                     (get-map-neighbours maze
                                                         current-position))))

                    (loop for neighbour in neighbours do
                      (let ((neighbour-distance (get-distance neighbour)))
                        (setf (gethash neighbour distances)
                                 (max (+ 1 current-distance)
                                      neighbour-distance))))

                    (queue/push-each unvisited neighbours)

                    (set-visited! current-position)

                    (search-from (queue/pop unvisited)))))))
      (search-from starting-point)))))

#+nil
(let ((explorer (make-instance 'idea-longest-path-breadth-first-explorer))
      (maze (load-grid-from-file "example.txt")))
  (search-path explorer maze (find-starting-position-grid maze))
  (gethash (vec2 21.0 22.0) (distances explorer)))

(defun longest-hike (grid
                     unvisited
                     get-neighbours-fn
                     mark-visited-fn
                     distances)
  (protect-against-infinite-loop!)

  (if (queue/empty unvisited)
      "DONE"
      (let ((next-unvisited (queue/pop unvisited)))
          (let* ((current-distance (get-distance next-unvisited distances))
                 (neighbours (funcall get-neighbours-fn next-unvisited)))

            (loop for neighbour in neighbours do
              (incf-distance neighbour distances current-distance))

            (apply #'queue/push unvisited neighbours)

            (funcall mark-visited-fn next-unvisited unvisited)

          (longest-hike grid
                        unvisited
                        get-neighbours-fn
                        mark-visited-fn
                        distances)))))

(defun load-grid-from-file (filename)
  (->> filename
       (get-inputs-pathname)
       (lib:read-file-lines)
       (lib:lists->2d-array)))

(defun set/append (items set &key (key #'identity) (test #'=))
  (reduce
   (lambda (res item)
     (adjoin item res :key key :test test))
   items
   :initial-value set))

;; SKETCH CODE

(defparameter *dnorth*  0)
(defparameter *deast* 1)
(defparameter *dsouth* 2)
(defparameter *dwest* 3)

(defun empty-cell (x y w h))
(defun wall-cell (x y w h)
  (sketch:with-pen (sketch:make-pen :fill sketch:+black+)
    (rect x y w h)))

(defun directed-cell (direction x y w h)
  (let ((tri-width (* 0.6 w))
        (tri-height (* 0.6 h)))

    (sketch:with-pen (sketch:make-pen :fill sketch:+yellow+ :weight 5)
      (rotated-isosceles-triangle (* 90 direction)
                                  (+ x (/ w 2))
                                  (+ y (/ (- h tri-height) 2))
                                  tri-width
                                  tri-height))))

(defsketch maze
    ((sketch:title "Day 23 - Maze")
     (sketch:width 800)
     (sketch:height 800)
     (iteration-count 0)
     (base-map-data #2A() :initarg :map)
     (distances (make-hash-table) :initarg distances)
     (visited (make-hash-table) :initarg visited)
     (current-cell NIL :initarg current-cell)
     (grid-canvas-dims '(800 800) :initarg grid-canvas-dims)
     (grid (make-grid (array-dimensions base-map-data)
                      (car grid-canvas-dims)
                      (cadr grid-canvas-dims))))
  (sketch:background (sketch:gray 0.2))

  (sketch:with-font (sketch:make-font :color sketch:+yellow+)
    (sketch:text (format nil "~D" iteration-count) 0 0))

  (incf iteration-count)

  (sketch:with-pen (sketch:make-pen :fill (sketch:gray 0.4)
                                    :stroke (sketch:gray 0.3)
                                    :weight 0.0)
    (sketch:with-translate (50 50)
      (let ((cell-width (grid/cell-width grid))
            (cell-height (grid/cell-height grid)))
        (destructuring-bind (h w) (array-dimensions base-map-data)
          (loop for j from 0 below h do
            (loop for i from 0 below w do
              (destructuring-bind (x y) (grid/canvas-xy-of-cell-at grid i j)
                (let ((cell (aref base-map-data j i)))
                  (cond
                    ((char= #\v cell) (directed-cell *dsouth* x y cell-width cell-height))
                    ((char= #\< cell) (directed-cell *dwest* x y cell-width cell-height))
                    ((char= #\> cell) (directed-cell *deast* x y cell-width cell-height))
                    ((char= #\# cell) (wall-cell x y cell-width cell-height))
                    ((char= #\. cell) (empty-cell x y cell-width cell-height))))))))

        (loop for vp in (alexandria:hash-table-keys visited) do
          (let ((val (gethash vp visited)))
            (let ((p (list-from-vec2 vp)))
              (with-translate-in-grid (grid :center (car p) (cadr p))
                (when val
                  (sketch:with-pen (sketch:make-pen :fill (sketch:gray 0.1))
                    (circle 0 0 (/ cell-width 2.5))))))))

        (when current-cell
          (with-translate-in-grid (grid :center (car current-cell) (cadr current-cell))
            (sketch:with-pen (sketch:make-pen :fill (sketch:gray 0.7))
              (circle 0 0 (/ cell-width 3)))))

        (when (> cell-width 10)
          (sketch:with-font (sketch:make-font :color sketch:+yellow+
                                              :align :center)
            (loop for vp in (alexandria:hash-table-keys distances) do
              (let ((val (gethash vp distances)))
                (let ((p (list-from-vec2 vp)))
                  (with-translate-in-grid (grid :center (car p) (cadr p))
                      (sketch:with-translate (0 -12.5)
                        (sketch:text (format nil "~D" val)
                                     0 0))))))))))))

(defun list-from-vec2 (v)
  (mapcar #'round (list (vx v) (vy v))))

;; END SKETCH CODE



(defun part1 (filename)
  (let* ((grid (load-grid-from-file filename))
         (start-pos (find-starting-position-grid grid))

         (sketch (make-instance 'maze :base-map-data grid
                                      :grid-canvas-dims '(800 800)
                                      :current-cell (list-from-vec2 start-pos))))

    (let ((explorer (make-instance 'idea-longest-tentative-distance-first-explorer)))
      (search-path explorer
                   grid
                   start-pos
                   NIL
                   :listener (lambda (event data)
                               (declare (ignore event))

                               (let ((current-position (list-from-vec2
                                                        (getf data :position))))

                                 (setf (slot-value sketch 'current-cell)
                                       current-position))

                               (setf (slot-value sketch 'distances)
                                     (getf data :distances))

                               (setf (slot-value sketch 'visited)
                                     (getf data :visited))

                               (sleep 0))

                   :commander (lambda (&rest args)
                                (declare (ignore args)))))))

    ;; (flet ((visitedp (pos) (gethash pos visited)))
    ;;   (longest-hike grid
    ;;                 unvisited
    ;;                 (lambda (pos)
    ;;                   (->> (get-map-neighbours grid pos)
    ;;                        (remove-if #'visitedp)))
    ;;                 (lambda (pos &rest args)
    ;;                   (declare (ignore args))

    ;;                   ;; redraw sketch
    ;;                   (setf (slot-value sketch 'current-cell)
    ;;                         (list-from-vec2 pos))

    ;;                   (setf (gethash pos visited) T))
    ;;                 distances))))

;;; GOTO HERE
#+nil
(part1 "example.txt")

#+nil
(loop for i from 0 to 10
      do
         (print i)
         (read-line))

#+nil
(make-instance 'maze :base-map-data (load-grid-from-file "example.txt")
               :grid-canvas-dims '(1080 1080)
               :current-cell '(3 1)

                     :distances (let ((d (make-hash-table :size 50 :test #'equalp)))
                                  (setf (gethash '(1 0) d) 0)
                                  (setf (gethash '(1 1) d) 1)
                                  (setf (gethash '(2 1) d) 2)
                                  d)
                     :visited (let ((d (make-hash-table :size 50 :test #'equalp)))
                                (setf (gethash '(2 1) d) T)
                                (setf (gethash '(1 1) d) T)
                                (setf (gethash '(1 0) d) T)
                                d))

#+nil
(loop for x from 0 to 10
      for y = (when (evenp x) x)
       when y collect y into ys
       when (> x 3) collect x into zs
       if (> x 3)
         collect y into ks
         and collect y into vs
      else
        collect y into vs
         finally (return (list ys zs ks vs)))

#+nil
(let ((hsh (make-hash-table :size 100 :test #'equal)))
  (macrolet ((add (k v) `(setf (gethash ,k hsh) ,v)))
    (add '(0 0) 9)
    (add '(1 3) 1)
    (add '(2 4) 2)
    (add '(3 4) 8)
    (add '(4 8) 3)
    (choose-unvisited-with-distance #'> '((3 4) (2 4) (0 0)) hsh)))

#+nil
(make-instance 'maze)

#+nil
(let ((hsh (make-hash-table :size 100 :test #'equal)))
  (macrolet ((add (k v) `(setf (gethash ,k hsh) ,v)))
    (add '(0 0) 9)
    (add '(1 3) 1)
    (add '(2 4) 2)
    (add '(3 4) 8)
    (add '(4 8) 3)
    (choose-unvisited-with-distance #'> '((3 4) (2 4) (0 0)) hsh)))

#+nil
(->> (load-grid-from-file "example.txt"))
