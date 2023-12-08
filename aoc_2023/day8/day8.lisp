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
               (setf (getmap hsh left-move  key) left)
               (setf (getmap hsh right-move key) right)
               ))
    hsh))

(defmacro peek-move (starting-location move map)
  `(gethash (make-map-key ,move ,starting-location) ,map))

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

(defun string-ends-with-p (str char)
  (char= (elt str (- (length str) 1))
         char))

;; subseq 4   = 1322249
;; subseq 2 4 = 1024459
;; subseq 0 2 = 992531
;; subseq 0 3 = 46648957
;;

;; (require :sb-sprof)
;; (sb-sprof:start-profiling)


(time (destructuring-bind ((movements) map-entry-strings) (lib:read-file-line-groups input)
  (let ((map (lines->map-entries map-entry-strings))
        (moves (make-instance 'circular-list :sequence movements)))
    (let ((starting-locations (subseq (remove-if-not (lambda (loc)
                                               (string-ends-with-p loc #\A))
                                             (all-map-locations map)) 0 2)))

    (loop
      with current-locations = starting-locations
      and current-move = (next moves)
      and n = 0
      until (every (lambda (loc) (string-ends-with-p loc #\Z)) current-locations)
      count
          (progn
            ;; (format t "~&Step: ~d~%" (incf n))
            (let ((next-locations (mapcar (lambda (loc)
                                            (peek-move loc current-move map))
                                  current-locations))
                  (next-move (next moves)))

              ;; (format t "~2&CURRENT: ~a~%NEXT: ~a~2%" current-locations next-locations)

              (setf current-locations next-locations)
              (setf current-move      next-move))))))))


;; (sb-sprof:report :type :flat)
