(in-package :aoc_2023)

(ql:quickload :fiveam)
(fiveam:def-suite things
  :description "Day 14")
(fiveam:in-suite things)

(fiveam:test test-things
  (fiveam:is (= 1 1)))

(fiveam:run! 'things)
