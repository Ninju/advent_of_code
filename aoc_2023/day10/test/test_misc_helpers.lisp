(in-package :aoc_2023)

(ql:quickload :fiveam)

(fiveam:def-suite misc-helpers-test-suite
  :description "Smaller, pure functions not worth their own file right now")

(fiveam:in-suite misc-helpers-test-suite)

(fiveam:test all-tests-go-here-to-start
  (fiveam:is (equal '(1 0) (reverse-direction '(-1 0))))
  (fiveam:is (equal (apply-directions '(3 3) '((1 0) (-1 0)))
                    '((4 3) (2 3))))

  (let ((*map* (build-map *example-2-input*)))
    (fiveam:is (equal '(0 2)
                      (find-starting-position))))

  (let ((*map* (build-map *example-1-input*)))
    (fiveam:is (equal '(1 1)
                      (find-starting-position)))))

(fiveam:run! 'all-tests-go-here-to-start)


(fiveam:test test-fn-direction-from
  (fiveam:is (equal '(-1 0) (direction-from '(3 4) '(2 4))))
  (fiveam:is (equal '(2 0) (direction-from '(0 4) '(2 4)))))

(fiveam:run! 'test-fn-direction-from)


(let ((*map* (build-map *example-2-input*)))
  (remove-if-not #'pipe-pos (get-neighbours (find-starting-position))))

(fiveam:run! 'misc-helpers-test-suite)
