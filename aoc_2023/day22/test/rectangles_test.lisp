(in-package :aoc_2023)

(ql:quickload :fiveam)

(fiveam:def-suite test-rectangles
  :description "Test rectangle functions")

(fiveam:in-suite test-rectangles)

(fiveam:test test-rectangles-overlap-p
  (fiveam:is (rectangles-overlap-p (make-rect '(0 0) '(10 10))
                                   (make-rect '(0 0) '(9 9))))

  ;; In Day 22, a rect of '(10 10) '(10 10) has volume 1
  (fiveam:is (rectangles-overlap-p (make-rect '(0 0) '(10 10))
                                   (make-rect '(10 10) '(10 10))))

  (fiveam:is (rectangles-overlap-p (make-rect '(9 9) '(10 10))
                                   (make-rect '(10 10) '(11 11))))

  (fiveam:is-false (rectangles-overlap-p (make-rect '(0 0) '(0 1))
                                         (make-rect '(1 1) '(1 2))))

  (fiveam:is-false (rectangles-overlap-p (make-rect '(0 0) '(10 10))
                                         (make-rect '(11 11) '(20 20))))


  )

(fiveam:test test-point-within-rect-p
  (fiveam:is (point-within-rect-p '(1 1)
                                  (make-rect '(0 0) '(9 9))))

  (fiveam:is (point-within-rect-p '(9 9)
                                  (make-rect '(0 0) '(9 9))))

  (fiveam:is (point-within-rect-p '(6 8)
                                  (make-rect '(5 8) '(9 9))))

  (fiveam:is-false (point-within-rect-p '(10 9)
                                        (make-rect '(0 0) '(9 9))))

  (fiveam:is-false (point-within-rect-p '(-1 0)
                                        (make-rect '(0 0) '(9 9))))

  )

(fiveam:run! 'test-rectangles)
