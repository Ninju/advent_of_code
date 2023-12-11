(in-package :aoc_2023)

(ql:quickload :fiveam)

(defparameter *fixture-example*
  (-> *example-input*
      (load-universe-from-file)))

(defparameter *example-input-expanded* (get-pathname "example_expanded.txt"))
(defparameter *fixture-example-expanded*
  (-> *example-input-expanded*
      (load-universe-from-file)
      (lib:lists->2d-array)))

(defun 2d-array= (a1 a2 &key (test #'eql))
  (let ((a1-dims (array-dimensions a1))
        (a2-dims (array-dimensions a2)))
    (if (not (equal a1-dims a2-dims))
        NIL
        (block outer
          (loop for j from 0 below (car a1-dims)
                do
                   (loop for i from 0 below (cadr a1-dims)
                         do
                            (if (not (funcall test
                                         (aref a1 j i)
                                         (aref a2 j i)))
                                (return-from 'outer NIL))))
          T))))

(fiveam:def-suite expanded-universe
  :description "Test the expansion of the universe.. according to Advent of Code Day 11")
(fiveam:in-suite expanded-universe)

(fiveam:test test-expanded-universe-with-fixtures
  (fiveam:is (2d-array= *fixture-example-expanded*
                    (convert-universe-to-2d-array
                     (expand-universe *fixture-example*)))))


(fiveam:run! 'expanded-universe)
