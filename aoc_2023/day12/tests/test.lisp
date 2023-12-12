(in-package :aoc_2023)

(ql:quickload :fiveam)

(fiveam:def-suite test-arrangements
  :description "Test different ways of arranging unknowns to match criteria")

(fiveam:in-suite test-arrangements)

(fiveam:test test-arrangements-n-with-knowns-only
             (fiveam:is (= 1
                           (arrangements-n "###" (list 3))))

             (fiveam:is (= 1
                           (arrangements-n "###..#" (list 3 1))))

             (fiveam:is (= 1
                           (arrangements-n "..#..###..#" (list 1 3 1))))
             )

(fiveam:run! 'test-arrangements-n-with-knowns-only)

(fiveam:test test-arrangements-n-with-unknowns-only
             (fiveam:is (= 3
                           (arrangements-n "???" (list 1))))
             )


(fiveam:run! 'test-arrangements-n-with-unknowns-only)


(fiveam:test test-arrangements-n-with-example-puzzle-input
             (fiveam:is (= 1
                           (arrangements-n "???,###" (list 1 1 3))))

             (fiveam:is (= 4
                           (arrangements-n ".??..??...?##." (list 1 1 3))))

             (fiveam:is (= 1
                           (arrangements-n "?#?#?#?#?#?#?#?" (list 1 3 1 6))))

             (fiveam:is (= 1
                           (arrangements-n "????.#...#..." (list  4 1 1))))

             (fiveam:is (= 4
                           (arrangements-n "????.######..#####." (list 1 6 5))))

             (fiveam:is (= 10
                           (arrangements-n "?###????????" (list 3 2 1)))))





(fiveam:run! 'test-arrangements-n)
