(in-package :aoc_2023)

(ql:quickload :alexandria)
(ql:quickload :arrows)
(ql:quickload :cl-ppcre)

(defparameter example-input #p"/home/alex/src/workspace/advent_of_code/aoc_2023/day8/example.txt")
(defparameter test-input #p"/home/alex/src/workspace/advent_of_code/aoc_2023/day8/test.txt")
(defparameter input #p"/home/alex/src/workspace/advent_of_code/aoc_2023/day8/input.txt")

(defparameter part2-example-input #p"/home/alex/src/workspace/advent_of_code/aoc_2023/day8/part2_example.txt")

(defparameter map-entry-partition-regex
  (ppcre:create-scanner "\\s+=\\s+"))

(defparameter map-entry-lhs-rhs-regex
  (ppcre:create-scanner "([0-9a-zA-Z]+)\\s*,\\s*([0-9a-zA-Z]+)"))

(defconstant left-move #\L)
(defconstant right-move #\R)

(defclass circular-list (standard-object)
  ((_cursor :initform 0)
   (_elements :initarg :sequence
              :initform (error "Must supply :sequence to circular-list"))
   _size))

(defmethod initialize-instance :after ((generator circular-list) &key)
  (setf (slot-value generator '_size)
        (length (slot-value generator '_elements))))

(defgeneric next (generator))
(defmethod next ((generator circular-list))
  (with-slots (_cursor _elements _size) generator
    (let ((next-element (elt _elements _cursor)))
      (if (>= (+ 1 _cursor) _size)
          (setf (slot-value generator '_cursor) 0)
          (incf (slot-value generator '_cursor)))
      next-element)))

(defgeneric peek (generator))
(defmethod peek ((generator circular-list))
  (with-slots (_cursor _elements _size) generator
    (let ((next-element (elt _elements _cursor)))
      next-element)))

(defun partition-by-equals (str)
  (ppcre:split map-entry-partition-regex str))

(defun parse-map-entry-line (line)
  "Line such as \"AAA = (BBB, CCC)\" becomes (\"AAA\" \"BBB\" \"CCC\")"
  (destructuring-bind (location left-right-string) (partition-by-equals line)
    (ppcre:register-groups-bind (lhs rhs)
        (map-entry-lhs-rhs-regex left-right-string)
      (list location lhs rhs))))

 (ppcre:all-matches-as-strings "\(([a-zA-Z]+)\\s*,\\s*([a-zA-Z]+)\)" "(BBB,CCC)")

(defun lines->map-entries (lines)
  (let* ((hsh (make-hash-table :test #'equal))
         (map-entries (mapcar #'parse-map-entry-line lines)))
    (loop for entry in map-entries
          do
             (destructuring-bind (key left right) entry
               (setf (gethash key hsh) (list left right))))
    hsh))

(defmacro peek-move (starting-location move map)
  (let ((g-exists (gensym))
        (g-val (gensym)))
  `(destructuring-bind (,g-val ,g-exists) ,(gethash starting-location map)
     (if ,g-exists
         (if (char= ,move ,left-move)
             (car ,g-val)
             (cadr ,g-val))))))

(defun remove-first-char (str)
  (subseq str 1))

(defun all-map-locations (map)
  (let ((keys (alexandria:hash-table-keys map)))
    (remove-duplicates keys
                       :test #'equal)))

(defun location-ends-with-p (location-str char)
  (char= (elt location-str 2)
         char))

(defparameter *testing-moves* "LRLRRR")
(defparameter *testing-circular-list* (make-instance 'circular-list :sequence *testing-moves*))

;; subseq 4   = 1322249
;; subseq 2 4 = 1024459
;; subseq 0 2 = 992531
;; subseq 0 3 = 46648957
;;

;; (require :sb-sprof)
;; (sb-sprof:start-profiling)

(defun next-location (move map)
  (lambda (loc)
    (let ((nodes (gethash loc map)))
      (if (equal current-move
                 left-move)
          (car nodes)
          (cadr nodes)))))

(defun get-left (map location)
  (car (gethash location map)))

(defun get-right (map location)
  (cadr (gethash location map)))

(defun parse-moves-to-lambdas (moves)
  (map 'vector
       (lambda (c)
         (if (char= c #\L)
             #'get-left
             (if (char= c #\R)
                 #'get-right
                 (error
                  (format nil "Unexpected char: '~a', only 'L' and 'R' accepted" c)))))
       moves))

(defun step-through (map current-depth locations moves)
  (let ((zs (remove-if-not (lambda (loc) (location-ends-with-p loc #\Z))
                           locations)))
    ;; (if (> (length zs) 0)
    ;;     (format t
    ;;             "~&Found ~d locations ending Z at depth ~d -- locations = ~a~%"
    ;;             (length zs)
    ;;             current-depth
    ;;             locations))
    (if (= (length locations) (length zs))
        (progn
          ;; (print locations)
          ;; (print current-depth)
          current-depth)
        (let* ((next-move (next moves))
               (new-locations (mapcar
                               (lambda (loc) (funcall next-move map loc))
                               locations)))
          ;; (print next-move)
          ;; (print new-locations)
          (step-through map (+ 1 current-depth) new-locations moves)))))

(time (let ((result
        (destructuring-bind ((movements) map-entry-strings) (lib:read-file-line-groups input)
          (let* ((map (lines->map-entries map-entry-strings))
                 (moves (make-instance 'circular-list :sequence
                                       (parse-moves-to-lambdas movements)))
                 (starting-locations (remove-if-not (lambda (loc)
                                                      (location-ends-with-p loc #\A))
                                                    (all-map-locations map))))
            (format t "Starting new run, starting at: ~a~%" starting-locations)
            (mapcar (lambda (location)
                      (step-through map 0 (list location) moves))
                    starting-locations)))))
        (format t "~2&The result is: ~d~%" (apply #'lcm result))))

 ; => 14265111103729 (44 bits, #xCF95AE148F1)
  ;   5,662,617,149




;; (sb-sprof:report :type :flat)
