(in-package :aoc_2023)

(ql:quickload :fiveam)

(defun seq-points= (seq1 seq2)
  (equal (sort seq1 #'sort-points)
         (sort seq2 #'sort-points)))

(fiveam:def-suite get-connected-neighbours-test-suite
  :description "Test functions needed to complete the puzzle")

(fiveam:in-suite get-connected-neighbours-test-suite)




(fiveam:test testing-fn-get-connected-neighbours-from-starting-position
  (let ((*map* (build-map *example-1-input*)))
    (fiveam:is (seq-points= (get-connected-neighbours '(1 1))
                      '((1 2) '(2 1)))))

  (let ((*map* (build-map *example-2-input*)))
    (fiveam:is (seq-points= (get-connected-neighbours '(0 2))
                      '((0 3) (1 2))))))

(fiveam:test testing-fn-get-connected-neighbours-from-starting-position
  (let ((*map* (build-map *example-1-input*)))
    (fiveam:is (seq-points= '((2 1) (1 2))
                            (get-connected-neighbours (find-starting-position)))))

  (let ((*map* (build-map *example-2-input*)))
    (fiveam:is (seq-points= '((0 3) (1 2))
                            (get-connected-neighbours (find-starting-position))))))




(fiveam:test testing-fn-get-connected-neighbours-from-pipe-to-starting-position
  (let ((*map* (build-map *example-1-input*)))
    (fiveam:is (seq-points= (get-connected-neighbours '(2 1))
                      '((1 1) (3 1))))
    (fiveam:is (seq-points= (get-connected-neighbours '(1 2))
                      '((1 1) (1 3)))))

  (let ((*map* (build-map *example-2-input*)))
    (fiveam:is (seq-points= (get-connected-neighbours '(1 2))
                      '((0 2) (1 1))))
    (fiveam:is (seq-points= (get-connected-neighbours '(0 3))
                      '((0 2) (0 4))))))

(fiveam:run! 'testing-fn-get-connected-neighbours-from-pipe-to-starting-position)




(fiveam:test testing-fn-get-connected-neighbours-from-pipe-to-pipe
  (let ((*map* (build-map *example-1-input*)))
    (fiveam:is (seq-points= (get-connected-neighbours '(2 3))
                      '((3 3) (1 3))))
    (fiveam:is (seq-points= (get-connected-neighbours '(1 3))
                      '((2 3) (1 2))))
    (fiveam:is (seq-points= (get-connected-neighbours '(3 1))
                      '((2 1) (3 2)))))

  (let ((*map* (build-map *example-2-input*)))
    (fiveam:is (seq-points= (get-connected-neighbours '(1 1))
                      '((2 1) (1 2))))
    (fiveam:is (seq-points= (get-connected-neighbours '(1 3))
                      '((1 4) (2 3))))))

(fiveam:run! 'testing-fn-get-connected-neighbours-from-pipe-to-pipe)

(fiveam:run! 'get-connected-neighbours-test-suite)
