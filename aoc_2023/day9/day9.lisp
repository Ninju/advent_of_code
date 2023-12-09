(in-package :aoc_2023)

(ql:quickload :alexandria)
(ql:quickload :arrows)
(ql:quickload :cl-ppcre)

(defparameter example-input #p"/home/alex/src/workspace/advent_of_code/aoc_2023/day9/example.txt")
(defparameter input #p"/home/alex/src/workspace/advent_of_code/aoc_2023/day9/input.txt")

(defun consecutive (lst)
  (loop for idx from 0 below (- (length lst) 1)
        collect
        (list (elt lst idx)
              (elt lst (+ 1 idx)))))

(defun differences (lst)
  (mapcar (lambda (e) (- (cadr e) (car e))) (consecutive lst)))

(defun zero-p (n)
  (= n 0))

(defun last-element (lst)
  (car (last lst)))

(defun first-element (lst)
  (car lst))

(defun next-in-series (lst)
  (if (every #'zero-p lst)
      0
      (let ((diffs (differences lst)))
        (+ (last-element lst)
           (next-in-series diffs)))))

(defun prior-in-series (lst)
  (if (every #'zero-p lst)
      0
      (let ((diffs (differences lst)))
        (- (first-element lst)
           (prior-in-series diffs)))))


;; (differences '(0 3 6 9 12 15 18))
;; (next-in-series '(0 3 6 9 12 15 18))

;; (differences '(0 3 6 9 12 15))
;; (differences '(3 3 3 3 3))
;; (differences '(0 0 0 0)) ;; => 0
;; (next-in-series '(0 3 6 9 12 15 18))

;; (next-in-series '(1 3 6 10 15 21))
;; (next-in-series '(10 13 16 21 30 45))


;; (next-in-series '(0 3 6 9 12 15))
;; (next-in-series '(3 3 3 3 3))
;; (next-in-series '(0 0 0 0))

(defun part1 (filepath)
  (loop for line in (lib:read-file-lines filepath)
        sum
        (arrows:-> line
                   (lib:extract-numbers-from-line)
                   (next-in-series))))

;; PART 2

(defun part2 (filepath)
  (loop for line in (lib:read-file-lines filepath)
        sum
        (arrows:-> line
                   (lib:extract-numbers-from-line)
                   (prior-in-series))))

(part2 input)
