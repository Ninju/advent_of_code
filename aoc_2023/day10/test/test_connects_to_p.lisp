(in-package :aoc_2023)

(ql:quickload :fiveam)

(fiveam:def-suite connects-to-p-test-suite
  :description "Test functions needed to complete the puzzle")

(fiveam:in-suite connects-to-p-test-suite)

;; PIPE TO STARTING POSITIONS
(fiveam:test testing-fn-connects-to-p-starting-position-to-point
  (let ((*map* (build-map *example-1-input*)))
    (fiveam:is (connects-to-p '(1 1) '(2 1)))
    (fiveam:is (connects-to-p '(1 1) '(1 2)))

    (fiveam:is-false (connects-to-p '(1 1) '(0 0)))
    (fiveam:is-false (connects-to-p '(1 1) '(0 1))))

  (let ((*map* (build-map *example-2-input*)))
    (fiveam:is (connects-to-p '(0 2) '(1 2)))
    (fiveam:is (connects-to-p '(0 2) '(0 3)))))

(fiveam:run! 'testing-fn-connects-to-p-starting-position-to-point)



;; PIPE TO PIPE
(fiveam:test testing-fn-connects-to-p-from-pipe-to-pipe
  (let ((*map* (build-map *example-1-input*)))
    (fiveam:is (connects-to-p '(3 3) '(2 3)))
    (fiveam:is (connects-to-p '(3 2) '(3 1))))

  (let ((*map* (build-map *example-2-input*)))
    (fiveam:is (connects-to-p '(0 4) '(1 4)))
    (fiveam:is (connects-to-p '(0 4) '(0 3)))

    (fiveam:is-false (connects-to-p '(0 3) '(1 3)))
    (fiveam:is-false (connects-to-p '(1 3) '(0 3)))))

(fiveam:run! 'testing-fn-connects-to-p-from-pipe-to-pipe)


;; RUN WHOLE SUITE
(fiveam:run! 'connects-to-p-test-suite)
