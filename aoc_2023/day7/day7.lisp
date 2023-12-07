(in-package :aoc_2023)

(defparameter example-input "/home/alex/src/workspace/advent_of_code/aoc_2023/day7/example.txt")
(defparameter input #p"/home/alex/src/workspace/advent_of_code/aoc_2023/day7/input.txt")

(defparameter *card-ranking* (reverse "AKQJT98765432"))


(ql:quickload :arrows)
(ql:quickload :cl-ppcre)

(defun parse-hand-and-bid (line)
  (ppcre:register-groups-bind (hand bid)
      ("([^ ]+)\\s+([^ ]+)+" line)
    (list hand (parse-integer bid))))

(defun five-of-a-kind-p (hand)
  (arrows:-> hand
             (remove-duplicates)
             (length)
             (= 1)))

(defun four-of-a-kind-p (hand)
  (let ((unique-chars (remove-duplicates hand)))
    (loop for c across unique-chars
          do
             (if (= 4 (count c hand))
                 (return T)))))

(defun full-house-p (hand)
  (let ((unique-chars (remove-duplicates hand)))
    (arrows:as-> (loop for c across unique-chars
                      collect
                      (count c hand)) counts
                (or (equal '(2 3) counts) (equal '(3 2) counts)))))

(defun three-of-a-kind-p (hand)
  (let ((unique-chars (remove-duplicates hand)))
    (loop for c across unique-chars
          do
             (if (= 3 (count c hand))
                 (return T)))))

(defun two-pair-p (hand)
  (let ((unique-chars (remove-duplicates hand)))
    (arrows:->> (loop for c across unique-chars
                      collect
                      (count c hand))
                (count 2)
                (= 2))))

(defun one-pair-p (hand)
  (let ((unique-chars (remove-duplicates hand)))
    (loop for c across unique-chars
          do
             (if (= 2 (count c hand))
                 (return T)))))

(defun rank (hand)
  (if (five-of-a-kind-p hand)
      100
      (if (four-of-a-kind-p hand)
          99
          (if (full-house-p hand)
              98
              (if (three-of-a-kind-p hand)
              97
                  (if (two-pair-p hand)
                      96
                      (if (one-pair-p hand)
                          95
                          10)))))))

(one-pair-p "AKQQJ")

(defun compare-rank (h1 h2)
  (let ((rank-a (rank h1))
        (rank-b (rank h2)))
    (if (= rank-a rank-b)
        (compare-hands-card-by-card h1 h2)
        (> rank-a rank-b))))

(defun card-by-card-rankings (h1 h2)
  (map 'vector (lambda (a b)
                 (cons (rank-card a)
                       (rank-card b))) h1 h2))

(defun compare-hands-card-by-card (h1 h2)
  (block result
    (loop for ranking across (card-by-card-rankings h1 h2)
          do
             (if (not (= (car ranking) (cdr ranking)))
                 (return-from result (> (car ranking) (cdr ranking)))))
    T))

;; (card-by-card-rankings "KJJTT" "KK677")
;; (compare-rank "KJJTT" "KK677")
;; (compare-rank "KK677" "KJJTT")

;; (compare-rank "36325" "26557")
;; (rank "36325")
;; (rank "26557")
;; (card-by-card-rankings "36325" "26557")

(defun rank-card (card)
  (position card *card-ranking*))

(defun rank-high-card (hand)
  (reduce #'max hand :key #'rank-card))

(loop for line in (lib:read-file-lines example-input)
      collect
      (destructuring-bind (hand bid) (parse-hand-and-bid line)
        (list hand bid)))

(defun sort-bids-from-input (input-file)
  (arrows:-<>> (lib:read-file-lines input-file)
               (mapcar #'parse-hand-and-bid)
               (stable-sort <> #'(lambda (a b) (compare-rank (car a) (car b))))))

  (arrows:-<>> (lib:read-file-lines input)
               (mapcar #'parse-hand-and-bid)
               (mapcar #'car)
               (stable-sort <> #'compare-rank)
               (mapcar (lambda (h)
                         (format t "~&~a (~d)~%" h
                                 (rank h)))))

(let* ((input-file input)
       (bids (sort-bids-from-input input-file))
       (number-of-hands (length bids)))
  (loop for n from 1 to number-of-hands
        sum
        (destructuring-bind (hand bid) (elt bids (- n 1))
          (let ((row-number-rank (+ 1 (- number-of-hands n))))
  ;;           (cons hand row-number-rank))))))
          (* bid row-number-rank)))))

 ; => 248453531 (28 bits, #xECF199B)
 ; => 252549368 (28 bits, #xF0D98F8) (if changing to (compare-rank (car b) (car a))

;  => 248451132 (28 bits, #xECF103C) TOO LOW
;  => 249965703 (28 bits, #xEE62C87) TOO HIGH
;; (arrows:-> (list '("CCCCC" "AAAAB" "AABAB" "AACAB" "CABAB" "CKBAB" "CKBAD"))
;;            (sort #'> :key #'rank))

(compare-rank "77888" "77788") ;; => T
(compare-rank "77788" "77888") ;; => NIL

(compare-rank "33332" "2AAAA") ;; => T
(compare-rank "2AAAA" "33332") ;; => NIl

(stable-sort #(1 2 4 3 1)
      #'(lambda (a b)
          T))
