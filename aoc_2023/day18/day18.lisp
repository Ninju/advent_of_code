(in-package :aoc_2023)

(ql:quickload :arrows)
(ql:quickload :cl-ppcre)

(use-package :arrows)

(declaim (optimize (debug 3)))

(defparameter *north* '(0 -1))
(defparameter *south* '(0  1))
(defparameter *west*  '(-1  0))
(defparameter *east*  '(1 0))

(defparameter *default-directions* (list *north*
                                         *east*
                                         *south*
                                         *west*))

(defparameter *day-number* 18)

(defparameter *map* NIL)
(defparameter *history* NIL)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-inputs-pathname (filename)
    (-> filename
        (merge-pathnames (format nil "day~d/inputs/" *day-number*))
        (merge-pathnames #p"/home/alex/src/workspace/advent_of_code/aoc_2023/"))))

(defun array-map (function array
                  &optional (retval (make-array (array-dimensions array))))
  "Apply FUNCTION to each element of ARRAY.
Return a new array, or write into the optional 3rd argument."
  (dotimes (i (array-total-size array) retval)
    (setf (row-major-aref retval i)
          (funcall function (row-major-aref array i)))))

(defun reduce-multidimensional-array (fn arr &rest args)
  (apply #'reduce
         fn
         (make-array (array-total-size arr) :displaced-to arr)
         args))

(defun collect-array-positions (arr fn)
  (let ((result '()))
    (destructuring-bind (h w) (array-dimensions arr)
      (loop for y from 0 below h
            do
               (loop for x from 0 below w
                     do
                        (let ((cell (aref arr y x)))
                          (if (funcall fn cell)
                              (push (make-point x y) result))))))
    result))

(defun make-point (x y) (list x y))
(defun x-coord (pos) (destructuring-bind (x _y) pos (declare (ignore _y)) x))
(defun y-coord (pos) (destructuring-bind (_x y) pos (declare (ignore _x)) y))
(defun add-points (p1 p2)
  (list (+ (x-coord p1) (x-coord p2))
        (+ (y-coord p1) (y-coord p2))))

(defun scale-point (p scalar)
  (list (* (x-coord p) scalar)
        (* (y-coord p) scalar)))

(defun absolute-horizontal-distance-between-points (p1 p2)
  (abs (- (x-coord p2)
          (x-coord p1))))

(defun out-of-bounds-p (pos)
  (destructuring-bind (h w) (array-dimensions *map*)
    (destructuring-bind (x y) pos
      (or (< x 0)
          (>= x w)
          (< y 0)
          (>= y h)))))

(defun all-points-between (p1 p2)
  (let ((max-x (max (x-coord p1) (x-coord p2)))
        (min-x (min (x-coord p1) (x-coord p2)))
        (max-y (max (y-coord p1) (y-coord p2)))
        (min-y (min (y-coord p1) (y-coord p2))))

    (let ((x-range (loop for n from (+ 1 min-x) below max-x collect n))
          (y-range (loop for n from (+ 1 min-y) below max-y collect n)))

      (cond
        ((not y-range) (mapcar (lambda (x) (make-point x min-y))
                               x-range))

        ((not x-range) (mapcar (lambda (y) (make-point min-x y))
                               y-range))

        (t (mapcar #'make-point x-range y-range))))))

(defun make-instruction (direction amount &optional color)
  (make-instance 'instruction
                 :direction direction
                 :amount amount
                 :color color))

(defclass instruction (standard-object)
  ((direction :initarg :direction :reader direction)
   (color :initarg :color :reader color)
   (amount :initarg :amount :reader amount)))

(defun move (pos instruction)
  (add-points pos
              (scale-point (direction instruction) (amount instruction))))

(defun accumulate-path (start-pos instructions)
  (reduce (lambda (path instr)
            (cons (move (car path) instr)
                  path))
          instructions
          :initial-value (list start-pos)))

(defun make-rectangle (lower-bound upper-bound) (list lower-bound upper-bound))

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
      (list (make-point lower-x lower-y)
            (make-point upper-x upper-y)))))

(defun find-point-at-x-boundary (path)
  (let ((rectangle-bounds (rectangle-bounds-of-path path)))
    (destructuring-bind ((lower-x _lower-y) (upper-x _upper-y)) rectangle-bounds
      (declare (ignore _lower-y) (ignore _upper-y))
      (loop for pos in path
            do
               (if (or (= (x-coord pos) lower-x)
                       (= (x-coord pos) upper-x))
                   (return pos))))))

(defun ensure-between (val lower upper)
  (let* ((diff (- upper lower))
         (dir (/ diff diff)))
    (cond
      ((= lower val) (+ val dir))
      ((= upper val) (- val 1))
      ((< val lower) (* dir (+ 1 lower)))
      ((> val upper) (* dir (+ 1 upper)))
      (t val))))

(defun translate-point-to-positive-coords-with-bounding-rectangle (pos rect)
  (destructuring-bind ((lower-x lower-y) _upper-bounds) rect
    (declare (ignore _upper-bounds))
    (let ((translation-point (make-point (* -1 lower-x) (* -1 lower-y))))
      (add-points pos translation-point))))

(defun translate-path-to-positive-coords (path)
  (let ((bounds (rectangle-bounds-of-path path)))
    (destructuring-bind ((lower-x lower-y) _upper-bounds) bounds
      (declare (ignore _upper-bounds))
      (let* ((translation-point (make-point (* -1 lower-x) (* -1 lower-y))))
        (loop for p in path
              collect
              (add-points p translation-point))))))

(defun fill-in-path-gaps (path)
  (reduce (lambda (new-path pos)
            (let* ((prev (car new-path))
                   (points (all-points-between prev pos)))
              (cons pos (append points new-path))))
          (cdr path)
          :initial-value (list (car path))))

(defun get-point-within-path (path)
  (block outer
    (let ((rectangle (rectangle-bounds-of-path path)))
      (destructuring-bind ((lower-x lower-y) (upper-x upper-y)) rectangle
        (loop for y from lower-y to upper-y
              do
                 (loop for x from lower-x to upper-x
                       do
                          (let ((current (make-point x y)))
                            (if (member current path :test #'equal)
                                (if (member (add-points (make-point 0 1)
                                                        current)
                                            path :test #'equal)
                                    ;; Hit a top-left corner
                                    (return-from outer
                                      (add-points (make-point 1 1)
                                                  current)))))))))))


(defun apply-directions (pos directions)
  (mapcar (lambda (d) (add-points pos d)) directions))

(defun get-neighbours (pos directions) (apply-directions pos directions))

(defun mark-location-as-visited (location visited)
  (push location *history*)
  (setf (aref visited (y-coord location) (x-coord location)) 1))

(defun location-was-visited (location visited)
  (equal 1 (aref visited (y-coord location) (x-coord location))))

(defun _depth-first-search (start-pos grid neighbour-filter-fn visited path unvisited)
  (let* ((neighbours (get-neighbours start-pos *default-directions*))
         (filtered-neighbours (remove-if neighbour-filter-fn neighbours))
         (unvisited-neighbours (remove-if (lambda (n)
                                            (location-was-visited n visited))
                                          filtered-neighbours)))

    (if (not (location-was-visited start-pos visited))
        (mark-location-as-visited start-pos visited))

    (let ((new-unvisited (append unvisited-neighbours unvisited)))

      (if (not unvisited-neighbours)
          (if (not new-unvisited)
              (if (not path)
                  visited
                  (_depth-first-search (car path)
                                       grid
                                       neighbour-filter-fn
                                       visited
                                       (cdr path)
                                       unvisited))
              (_depth-first-search (car new-unvisited)
                                   grid
                                   neighbour-filter-fn
                                   visited
                                   path
                                   (cdr new-unvisited)))
          (_depth-first-search (car unvisited-neighbours)
                               grid
                               neighbour-filter-fn
                               visited
                               (cons start-pos path)
                               new-unvisited)))))

(defun depth-first-search (start-pos grid neighbour-filter-fn path)
  (let ((visited (make-array (array-dimensions grid))))
    (_depth-first-search start-pos grid neighbour-filter-fn visited path '())))

(defun array-from-path (path &rest args)
  (let* ((bounding-rect (rectangle-bounds-of-path path))
         (dims (->> bounding-rect
                    (rectangle-dims)
                    (reverse)       ;; because rect is (x y) but array is (h w)
                    (pad-dims 1)))) ;; 0 indexing: (0 0) to (10 10) is 11 in xy
    (apply #'make-array dims args)))


(defun search-points-within-path (path)
  (let* ((positive-path (translate-path-to-positive-coords path))
         (sealed-path (fill-in-path-gaps positive-path))
         (step-into-point (get-point-within-path sealed-path))
         (starting-grid (array-from-path sealed-path))
         (*map* starting-grid))

    (depth-first-search step-into-point
                        starting-grid
                        (lambda (pos)
                          (or (member pos sealed-path :test #'equal)
                              (out-of-bounds-p pos)))
                        '())))

(defun parse-movement (str)
  (cond
    ((string= str "R") *east*)
    ((string= str "U") *north*)
    ((string= str "D") *south*)
    ((string= str "L") *west*)))

(defun parse-instruction (string)
  (destructuring-bind (movement amount color) (ppcre:split "\\s+" string)
    (make-instruction (parse-movement movement)
                      (parse-integer amount)
                      color)))

(defun pad-dims (n dims)
  (mapcar (lambda (d) (+ n d)) dims))

(defun draw-points-on-array (non-empty-char empty-char points)
  (let ((canvas (array-from-path points :initial-element empty-char)))
    (loop for pos in points
          do
             (setf (aref canvas
                         (y-coord pos)
                         (x-coord pos))
                   non-empty-char))
    canvas))

(defun draw-path (path)
  (->> path
       (fill-in-path-gaps)
       (translate-path-to-positive-coords)
       (draw-points-on-array #\# #\.)))

(defun draw-array-of-chars (array)
  (loop for i below (car (array-dimensions array)) do
    (loop for j below (cadr (array-dimensions array)) do
      (let ((cell (aref array i j)))
        (format t "~a" cell)))
    (format t "~%")))

(defun load-instructions-from-file (filename)
  (mapcar #'parse-instruction
          (lib:read-file-lines (get-inputs-pathname filename))))

(defun step-forward-history (grid cursor painter)
  (if (not (>= cursor (length *history*)))
      (funcall painter (elt *history* cursor) grid)))

(defun replay-history (starting-grid &optional (sleep-t 0))
  (loop for n from (- (length *history*) 1) downto 0
        do

           (step-forward-history starting-grid n
                                 (lambda (historical-val grid)
                                   (setf (aref grid
                                               (y-coord historical-val)
                                               (x-coord historical-val))
                                         #\#)))

           (format t "~2&")

           (draw-array-of-chars starting-grid)

           (sleep sleep-t)))

(defun part1 (filename)
  (let* ((instructions (load-instructions-from-file filename))
         (trench (accumulate-path '(0 0) instructions))
         (interior-location-map (search-points-within-path trench))
         (interior-locations (collect-array-positions interior-location-map
                                                      (lambda (c) (= 1 c)))))
    (+ -1 ;; initial hole in trench not counted
       (length interior-locations)
       (length (fill-in-path-gaps trench)))))

(defun make-line (start end) (sort (list start end) #'< :key #'y-coord))

(defun line-start (line)
  (destructuring-bind (start _end) line
    (declare (ignore _end))
    start))

(defun line-end (line)
  (destructuring-bind (_start end) line
    (declare (ignore _start))
    end))

(defun line-start-x (line) (x-coord (line-start line)))
(defun line-start-y (line) (y-coord (line-start line)))
(defun line-end-x (line) (x-coord (line-end line)))
(defun line-end-y (line) (y-coord (line-end line)))
(defun vertical-line (line) (= (line-start-x line) (line-end-x line)))

(defun sort-lines-by-x-asc (lines)
  (loop for line in lines
        collect (sort line #'< :key #'car)))

(defun sort-lines-by-y-asc (lines) (sort lines #'< :key (lambda (l)
                                                          (-> l
                                                              (cadr)
                                                              (y-coord)))))

(defun instructions->lines (start-pos instructions)
  (let ((current-pos start-pos)
        (lines '()))

    (loop for instruction in instructions
          do
             (let ((line-end-point (move current-pos instruction)))

               (push (make-line current-pos line-end-point) lines)
               (setf current-pos line-end-point)))
    lines))

(defun line-min-y-coord (line)
  (destructuring-bind (start end) line
    (min (y-coord start) (y-coord end))))

(defun sort-lines-by-min-y (lines)
  (sort lines #'< :key #'line-min-y-coord))

(defun lines-opposite-y-p (a b)
  (let ((a-start-x (line-start-x a))
        (a-start-y (line-start-y a))
        (a-end-x (line-end-x a))
        (a-end-y (line-end-y a))

        (b-start-x (line-start-x b))
        (b-start-y (line-start-y b))
        (b-end-x (line-end-x b))
        (b-end-y (line-end-y b)))

    (and (or ;; y-coord for A contained in B
          (between-p a-start-y b-start-y b-end-y)
          (between-p a-end-y   b-start-y b-end-y)
          (between-p b-start-y a-start-y a-end-y)
          (between-p b-end-y   a-start-y a-end-y))
         (not (or (= a-start-x b-start-x)
                  (= a-start-x b-end-x)
                  (= a-end-x b-start-x)
                  (= a-end-x b-end-x))))))

(defun rectangle-left-edge (rect)
  (destructuring-bind ((lower-x lower-y) (_upper-x upper-y)) rect
    (declare (ignore _upper-x))
    (make-line (make-point lower-x lower-y)
               (make-point lower-x upper-y))))

(defun rectangle-right-edge (rect)
  (destructuring-bind ((_lower-x lower-y) (upper-x upper-y)) rect
    (declare (ignore _lower-x))
    (make-line (make-point upper-x lower-y)
               (make-point upper-x upper-y))))

;; (defun lines-overlap-p (line1 line2)
;;   )

(defun split-line-by-rectangle (rect line)
  (destructuring-bind ((lower-x lower-y) (upper-x upper-y)) rect
    (if (not (or (= (line-start-x line) lower-x)
                 (= (line-start-x line) upper-x)))
        ;; line is not on the rectangle
        (list "NO NO")
        (let ((upper-match (if (< (line-start-y line) lower-y)
                               (make-line (make-point (line-start-x line)
                                                      (line-start-y line))
                                          (make-point (line-start-x line)
                                                      (- lower-y 1)))))
              (lower-match (if (> (line-end-y line) upper-y)
                               (make-line (make-point (line-start-x line)
                                                      (+ upper-y 1))
                                          (make-point (line-start-x line)
                                                      (line-end-y line))))))
          (remove nil (list upper-match lower-match))))))

(defun rectangle-between-parallel-vertical-lines (line adjacent-line)
  "Given LINE and ADJACENT-LINE returns a RECT and the REMAINING-LINES"

  (if (not (lines-opposite-y-p line adjacent-line))
      (progn (values NIL line adjacent-line)
             (error "This should not happen in this Advent of Code challenge."))

      (let ((rect (make-rectangle
                   (make-point (line-start-x line)
                               (max (line-start-y line)
                                    (line-start-y adjacent-line)))

                   (make-point (line-start-x adjacent-line)
                               (min (line-end-y line)
                                    (line-end-y adjacent-line))))))

        (values rect (remove nil (mapcan (lambda (l)
                                           (split-line-by-rectangle rect l))
                                         (list line adjacent-line)))))))

(defun rectangle-area (rect)
  (destructuring-bind ((lower-x lower-y) (upper-x upper-y)) rect
    (* (- upper-x lower-x)
       (- upper-y lower-y))))

(defun between-p (val lower upper &key (inclusive T))
  (if inclusive
      (and (<= lower val)
           (<= val upper))
      (and (< lower val)
           (< val upper))))

(defun choose-next-leftmost-pair (lines)
  (let* ((lines-by-x (stable-sort lines #'< :key #'line-start-x))
         (leftmost-line (car lines-by-x))

         (pair (find-if (lambda (candidate)
                          (lines-opposite-y-p leftmost-line candidate))
                        (cdr lines-by-x))))

    (if (not pair)
        (progn (format t "~2&---- CANNOT FIND LEFTMOST PAIR, LINES=~A" lines)
               (error "UNEXPECTED!")))

    (values (cons leftmost-line pair)
            (remove pair (cdr lines-by-x) :test #'equal))))

(defun point-on-line-p (point line)
  (and (between-p (x-coord point)
                  (line-start-x line)
                  (line-end-x line))
       (between-p (y-coord point)
                  (line-start-y line)
                  (line-end-y line))))

(defun draw-lines (empty-char line-char lines)
  (let ((canvas (array-from-path (mapcan #'identity (copy-tree lines))
                                 :initial-element empty-char)))

    (destructuring-bind (h w) (array-dimensions canvas)
      (loop for y from 0 below h
            do
               (loop for x from 0 below w
                     do
                        (if (some (lambda (l) (point-on-line-p (list x y) l))
                                  lines)
                            (setf (aref canvas y x) line-char)))))
    canvas))

(defun area-in-shape (lines &optional (recursion-limit 10000))
  (if (< recursion-limit 0)
      (error "Recursion limit reached")
      (if (or (not lines) (not (cdr lines)))
          0
          (multiple-value-bind (leftmost-pair rest-of-lines)
              (choose-next-leftmost-pair lines)

            (format t "~&LEFT=~A , RIGHT=~A~&   Chosen from: ~A"
                    (car leftmost-pair)
                    (cdr leftmost-pair)
                    lines)

            (multiple-value-bind (rect leftover-lines)
                (rectangle-between-parallel-vertical-lines (car leftmost-pair)
                                                           (cdr leftmost-pair))
              (+ (rectangle-area rect)
                 (area-in-shape (append leftover-lines rest-of-lines)
                                (- recursion-limit 1))))))))

#+nil
(let ((SLYNK-STICKERS:*BREAK-ON-STICKERS* (list :after)))
  (rectangle-area
   (rectangle-between-parallel-vertical-lines '((0 0) (0 500254)) '((461937 0) (461937 56407)))))
                                        ; => 26056480359 (35 bits, #x611165667)

#+nil
(let ((SLYNK-STICKERS:*BREAK-ON-STICKERS* (list :after)))
  (area-in-shape
   (sort-lines-by-x-asc
    (remove-duplicates
     (remove-if-not #'vertical-line
                    (instructions->lines '(0 0)
                                         (load-instructions-from-file "input.txt")))
     :test #'equal))))

#+nil
(progn
  (format t "~5&")
  (draw-array-of-chars
   (draw-lines #\. #\X
               ;; (remove-if-not #'vertical-line
               (instructions->lines '(0 0)
                                    (load-instructions-from-file "test3.txt")))))
)

;; (some (lambda (l) (point-on-line-p '(0 3) l)) '(((0 0) (0 2)) ((2 2) (2 5)) ((0 5) (0 7)) ((1 7) (1 9)) ((6 7) (6 9))
;; ((4 5) (4 7)) ((6 0) (6 5))))

#+nil
(loop for line in '(((2 2) (0 2)) ((9 3) (9 8)))
      collect (sort line #'< :key #'car))

#+nil
(split-line-by-rectangle (make-rectangle '(0 4) '(8 8))
                         (make-line '(0 1) '(0 10)))

#+nil
(split-line-by-rectangle (make-rectangle '(0 2) '(8 8))
                         (make-line '(0 0) '(0 2)))

#+nil
(* 461937 56407)
;; 26,056,480,359

#+nil
(part1 "example.txt")

#+nil
(part1 "input.txt")

;; CLEAR SCREEN
#+nil
(format t "~30&")

#+nil
(rectangle-between-parallel-vertical-lines '((0 0) (0 10)) '((19 0) (19 8)))

#+nil
(rectangle-between-parallel-vertical-lines '((0 0) (0 5)) '((19 0) (19 8)))

#+nil
(rectangle-between-parallel-vertical-lines '((0 0) (0 5)) '((19 0) (19 5)))

#+nil
(subtract-vertical-lines (make-line '(0 0) '(0 5))
                         (make-line '(8 0) '(8 3)))

#+nil
(remove-if-not
 #'vertical-line
 (sort-lines-by-x-asc
  (instructions->lines '(0 0) (load-instructions-from-file "test5.txt"))))

#+nil
(choose-next-leftmost-pair
 '(((0 0) (0 500254)) ((5411 500254) (5411 1186328)) ((461937 0) (461937 56407))
   ((497056 356353) (497056 1186328)) ((609066 356353) (609066 1186328))
   ((818608 56407) (818608 919647)) ((1186328 919647) (1186328 1186328))))


#+nil
(remove-duplicates (->> (load-instructions-from-file "test5.txt")
                        (accumulate-path '(0 0))
                        (sort-points-by-x))
                   :test #'equal)

#+nil
(let ((*history* '())
      (*test-path* (accumulate-path '(0 0) (load-instructions-from-file "input.txt"))))
  (->> *test-path*
       (fill-in-path-gaps)
       (reverse)
       (translate-path-to-positive-coords)
       (get-point-within-path)))

#+nil
(let ((*history* '())
      (*test-path* (accumulate-path '(0 0) (load-instructions-from-file "input.txt"))))
  (-> *test-path*
      (draw-path)
      (draw-array-of-chars)))

#+nil
(let ((*history* '())
      (*test-path* (accumulate-path '(0 0) (load-instructions-from-file "test3.txt"))))
  (-> (search-points-within-path *test-path*)
      (collect-array-positions (lambda (c) (= 1 c)))
      (append *test-path*)
      (draw-path)
      (draw-array-of-chars))

  (replay-history
   (draw-points-on-array #\# #\. (-> (fill-in-path-gaps *test-path*) (translate-path-to-positive-coords)))
   0.02))

#+nil
(let ((*map* (array-from-path *test-path*)))
  (out-of-bounds-p '(6 3)))

#+nil
(let ((*history* '((1 1) (1 0) (0 1) (0 2) (0 3))))
  (replay-history (array-from-path *test-path* :initial-element #\.) 1))

#+nil
(->> *test-path*
     (draw-path)
     (draw-array-of-chars))

#+nil
(fill-in-path-gaps '((0 0) (0 4) (10 4)))

#+nil
(all-points-between '(0 0) '(4 0))

#+nil
(all-points-between '(0 0) '(0 4))

#+nil
(defparameter *test-path* (accumulate-path '(0 0) (load-instructions-from-file "example.txt")))

#+nil
(lib:read-file-lines (get-inputs-pathname "example.txt"))

#+nil
(direction (parse-instruction "D 6 (#70c710)"))

#+nil
(get-neighbours '(0 0) (list *north* *east* *south* *west*))

#+nil
(get-neighbours '(0 0) (list *north* *east* *south* *west*))

#+nil
(translate-point-to-positive-coords-with-bounding-rectangle '(-9 -10)
                                                            (rectangle-bounds-of-path '((-10 -10) (-4 -4) (0 0) (1 1) (1 0))))

#+nil
(get-point-within-path '((-10 -10) (-4 -4) (0 0) (1 1) (1 0)))

#+nil
(find-points-at-x-boundary '((-10 -10) (-4 -4) (0 0) (1 1) (1 0)))

#+nil
(translate-path-to-positive-coords '((-10 -10) (-4 -4) (0 0) (1 1) (1 0)))

#+nil
(get-next-x-between '(10 0) 0 10)

#+nil
(find-point-at-x-boundary '((0 0) (0 10) (4 0) (9 8) (10 4) (9 0)))

#+nil
(rectangle-bounds-of-path '((0 0) (0 10) (4 0) (9 8) (10 4) (9 0)))

#+nil
(let ((instructions (mapcar (lambda (args)
                              (apply #'make-instruction args))
                            (list (list *north* 4) (list *east* 2) (list *south* 6)))))
  (accumulate-path '(0 0) instructions))

#+nil
(move
 (move '(0 0) (make-instruction *north* 2))
 (make-instruction *east* 3))

#+nil
(split-line-by-rectangle '((0 2) (2 2)) '((0 0) (0 2)))
