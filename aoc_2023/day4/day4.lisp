(in-package :aoc_2023)

(ql:quickload :split-sequence)
(ql:quickload :cl-ppcre)

(defparameter example-input "/home/alex/src/workspace/advent_of_code/aoc_2023/day4/example.txt")
(defparameter input "/home/alex/src/workspace/advent_of_code/aoc_2023/day4/input.txt")

(defun get-main-lines ()
  (lib:read-file-lines input))

(defun get-example-lines ()
  (lib:read-file-lines example-input))

(defparameter *lines* (get-example-lines))

(defparameter example-line "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19")

(defun extract-numbers-from-line (str)
  (mapcar #'parse-integer
          (ppcre:split "\\s+"
                       (string-trim " " str))))

(defun extract-winning-and-chosen-numbers (numbers-line)
  (destructuring-bind (winning-numbers chosen-numbers)
      (ppcre:split "\\|" numbers-line)
    (mapcar #'extract-numbers-from-line
            (list winning-numbers chosen-numbers))))

(defun extract-card-details (card-line)
  (ppcre:register-groups-bind (card-no numbers)
      ("Card\\s+([0-9]+):(\.+)"
       card-line)
    (cons card-no
          (extract-winning-and-chosen-numbers numbers))))

(defun winning-numbers-in-card (scratchcard)
  (destructuring-bind (card-no . (wins choices))
      scratchcard
    (lib:seq-intersection wins choices)))

(defun scratchcard-value (scratchcard)
  (length (winning-numbers-in-card scratchcard)))

(defun build-cards-db ()
  (let ((cards (make-array (length *lines*))))
    (loop for line in *lines*
          do
             (let* ((card (extract-card-details line))
                    (card-no (parse-integer (car card)))
                    (card-idx (- card-no 1)))
               (setf (elt cards card-idx) card)))
    cards))

(defun main ()
  (lib:sum (let ((cards-db (build-cards-db))
                 (copies-db (make-array (length *lines*) :initial-element 1)))
             (loop for scratchcard across cards-db
                   do
                      (let* ((wins (scratchcard-value scratchcard))
                             (card-no (parse-integer (car scratchcard)))
                             (number-copies-of-card (elt copies-db (- card-no 1))))
                        (format t "~&Card ~d won ~d times (had ~d copies)~%" card-no wins number-copies-of-card)
                        (loop for n from 1 to wins do
                          (let ((newly-won-card-no (+ card-no n)))
                            (if (not (> newly-won-card-no (length copies-db)))
                                (setf (elt copies-db (+ card-no n -1))
                                      (+ (* 1 number-copies-of-card) (elt copies-db (+ card-no n -1)))))))))
             copies-db)))

(let ((*lines* (get-main-lines)))
  (main))
