(in-package :aoc_2023)

(ql:quickload :split-sequence)
(ql:quickload :cl-permutation)
(ql:quickload :cl-ppcre)
(ql:quickload :alexandria)
(ql:quickload :arrows)

(use-package :arrows)

(defparameter *day-number* 13)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-inputs-pathname (filename)
    (arrows:-> filename
               (merge-pathnames (format nil "day~d/inputs/" *day-number*))
               (merge-pathnames #p"/home/alex/src/workspace/advent_of_code/aoc_2023/"))))

(defparameter *input* (get-inputs-pathname "input.txt"))

(defun note (&rest args) )
(defun not-implemented (&optional description &rest body)
  "Placeholder function for bits of code that aren't implemented yet"
  (error (format nil "Not implemented: ~a"
                 (if description description "no description given"))))

(defparameter +rock-char+ #\#)
(defparameter +ash-char+ #\.)
;; (defparameter *-regex* (ppcre:create-scanner "\\s+"))

(defun rock-p (c) (char= c +rock-char+))
(defun ash-p (c) (char= c +ash-char+))

(defun difference (list-1 list-2)
  (loop for l1 in list-1
        and l2 in list-2
        when (not (equal l1 l2))
          count 1))

(defun seq-difference (seq-1 seq-2)
  (loop for l1 across seq-1
        and l2 across seq-2
        when (not (equal l1 l2))
          count 1))

(defun parse-rocks-file (filename) (lib:read-file-line-groups filename))

(defun get-column (map x)
  (let ((height (car (array-dimensions map))))
    (loop for y from 0 below height
          collect
          (aref map y x))))

(defun find-candidate-mirror-vertical (map &key (test #'equal))
  (let ((as-2d-array (lib:lists->2d-array map)))
    (destructuring-bind (h w) (array-dimensions as-2d-array)
      (loop for x from 0 below (- w 1)
            when (funcall test
                  (get-column as-2d-array x)
                  (get-column as-2d-array (+ 1 x)))
            collect
            x))))

(defun find-candidate-mirror-horizontal (map &key (test #'equal))
  (loop for p in (lib:consecutive map)
        and idx from 0
        when (destructuring-bind (a b) p (funcall test a b))
          collect idx))

(defun check-mirror-horizontal (map y &key (required-mismatches 0)
                                        (test #'equal))
  (let ((mismatches 0))
    (block outer
      (loop for j from y downto 0
            do
               (let ((j2 (+ (- y j) 1 y)))
                 (if (>= j2 (length map))
                     (return-from outer (= mismatches required-mismatches))
                     (let ((lhs (elt map j))
                           (rhs (elt map j2)))
                       (if (not (funcall test lhs rhs))
                           (return-from outer NIL)
                           (progn
                             (incf mismatches (seq-difference lhs rhs))
                             (if (> mismatches required-mismatches)
                                 (return-from outer NIL))))))))
      (= mismatches required-mismatches))))

(defun check-mirror-vertical (map x &key (required-mismatches 0)
                                      (test #'equal))
  (let ((mismatches 0))
    (block outer
      (let ((as-2d-array (lib:lists->2d-array map))
            (width (reduce #'min map :key #'length)))
        (destructuring-bind (h w) (array-dimensions as-2d-array)
          (loop for i from x downto 0
                do
                   (let ((i2 (+ (- x i) 1 x)))
                     (if (>= i2 width)
                         (return-from outer (= mismatches required-mismatches))
                         (let ((lhs (get-column as-2d-array i))
                               (rhs (get-column as-2d-array i2)))
                           (if (not (funcall test lhs rhs))
                               (return-from outer NIL)
                               (progn
                                 (incf mismatches (difference lhs rhs))
                                 (if (> mismatches required-mismatches)
                                     (return-from outer NIL))))))))))
      (= mismatches required-mismatches))))

(defun find-mirrors (maps &key (allow-smudge NIL))
  (let ((test-fn-rows (if (not allow-smudge)
                          #'equal
                          (lambda (lhs rhs) (< (seq-difference lhs rhs)
                                               2))))

        (test-fn-cols (if (not allow-smudge)
                          #'equal
                          (lambda (lhs rhs) (< (difference lhs rhs)
                                               2))))

        (smudge (if allow-smudge 1 0)))

    (loop for map in maps
          collect
          (block continue
            (let ((candidate-horizontals
                    (find-candidate-mirror-horizontal map :test test-fn-rows)))
              (progn
                (loop for candidate in candidate-horizontals
                      do
                         (if (check-mirror-horizontal map candidate
                                                      :required-mismatches smudge
                                                      :test test-fn-rows)
                             (return-from continue (list :horizontal candidate))))

              (let ((candidate-verticals (find-candidate-mirror-vertical
                                          map
                                          :test test-fn-cols)))
                (loop for candidate in candidate-verticals
                      do
                         (if (check-mirror-vertical map candidate
                                                    :required-mismatches smudge
                                                    :test test-fn-cols)
                             (return-from continue (list :vertical candidate)))))))))))

(let ((n 0))
  (loop for mirror in (find-mirrors (parse-rocks-file (get-inputs-pathname "input.txt"))
                                    :allow-smudge T)
        do
           (if (not mirror)
               (error "MISING MIRROR!")
               (setf n (+ n
                          (if (equal (car mirror) :horizontal)
                              (prog1 (* 100 (+ 1 (cadr mirror))) (print mirror) (print (cadr mirror)))
                              (+ 1 (cadr mirror)))))))
  n)
