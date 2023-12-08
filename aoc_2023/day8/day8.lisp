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

(defun partition-by-equals (str)
  (ppcre:split map-entry-partition-regex str))

(defun parse-map-entry-line (line)
  "Line such as \"AAA = (BBB, CCC)\" becomes (\"AAA\" \"BBB\" \"CCC\")"
  (destructuring-bind (location left-right-string) (partition-by-equals line)
    (ppcre:register-groups-bind (lhs rhs)
        (map-entry-lhs-rhs-regex left-right-string)
      (list location lhs rhs))))

 (ppcre:all-matches-as-strings "\(([a-zA-Z]+)\\s*,\\s*([a-zA-Z]+)\)" "(BBB,CCC)")

(defun make-map-key (move location)
  (format nil "~a~a" move location))

(defun getmap (map move location)
  (let ((map-key (make-map-key move location)))
    (gethash map-key map)))

(defun setmap (map move location val)
  (let ((map-key (make-map-key move location)))
    (setf (gethash map-key map) val)))

(defsetf getmap setmap)

(defun lines->map-entries (lines)
  (let* ((hsh (make-hash-table :test #'equal))
         (map-entries (mapcar #'parse-map-entry-line lines)))
    (loop for entry in map-entries
          do
             (destructuring-bind (key left right) entry
               (format t "KEY: ~a, Left: ~a, Right: ~a~%" key left right)
               (setf (getmap hsh left-move  key) left)
               (setf (getmap hsh right-move key) right)
               ))
    hsh))

(defun peek-move (starting-location move map)
  (getmap map move starting-location))

(defparameter *test-hsh* (make-hash-table :test #'equal))

(defun _search-locations
    (number-locations-searched pred map starting-locations visited-hsh)
  (let ((locations
          (arrows:-<>> starting-locations
                      (remove-duplicates <> :test #'equal)
                      (remove-if (lambda (loc) (gethash loc visited-hsh)))
                      (remove-if pred))))
    (if (= 0 (length locations))
        number-locations-searched
        (_search-max-depth-to-locations
         (+ number-locations-searched (length locations))
         pred
         map
         (mapcan (lambda (loc)
                   (let ((left-loc  (peek-move loc left-move  map))
                         (right-loc (peek-move loc right-move map)))
                     (setf (gethash loc visited-hsh) T)
                     (list left-loc right-loc)))
                 locations)
         visited-hsh))))

(defun search-max-depth-to-locations (pred map starting-locations)
  (let ((visited-hsh (make-hash-table :test #'equal)))
    (_search-locations 0
                       pred
                       map
                       starting-locations
                       visited-hsh)))

(defun remove-first-char (str)
  (subseq str 1))

(defun all-map-locations (map)
  (let ((keys (alexandria:hash-table-keys map)))
    (remove-duplicates (mapcar #'remove-first-char keys)
                       :test #'equal)))

(destructuring-bind ((movements) map-entry-strings) (lib:read-file-line-groups input)
  (let ((map (lines->map-entries map-entry-strings)))
    (let ((starting-locations (remove-if-not (lambda (loc)
                                               (char= (elt loc (- (length loc) 1))
                                                      #\A))
                                             (all-map-locations map))))

    (search-max-depth-to-locations (lambda (loc)
                                     (char= (elt loc (- (length loc) 1)) #\Z))
                                   map
                                   starting-locations))))


    ;; (format t "~5&STARTING LOOP!~2%" current-move current-location)
    ;; (loop
    ;;   with current-location = "AAA"
    ;;   and current-move = (next moves)
    ;;   until (string= "ZZZ" current-location)
    ;;   count
    ;;       (progn
    ;;         (format t "~2&Next move: ~a, Current location: ~a~%" current-move current-location)
    ;;         (let ((next-location (peek-move current-location current-move map))
    ;;               (next-move (next moves)))

    ;;           (format t "Starting at ~a moving to ~a (having moved ~a)~%"
    ;;                   current-location
    ;;                   next-location
    ;;                   current-move)

    ;;           (setf current-location next-location)
    ;;           (setf current-move     next-move))))))
