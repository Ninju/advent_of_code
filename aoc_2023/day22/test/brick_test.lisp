(in-package :aoc_2023)

(ql:quickload :fiveam)

(fiveam:def-suite test-bricks
  :description "Test brick functions")

(fiveam:in-suite test-bricks)

(fiveam:test test-brick-volume
  (fiveam:is (= 0
                (brick-volume (make-brick (list 0 0 10) (list 0 0 10)))))
  (fiveam:is (= 1
                (brick-volume (make-brick (list 0 0 10) (list 0 0 10)))))
  (fiveam:is (= 1
                (brick-volume (make-brick (list 0 0 10) (list 0 1 10)))))
  (fiveam:is (= 10
                (brick-volume (make-brick (list 0 0 1) (list 0 0 10))))))

(fiveam:test test-brick-on-ground
  (fiveam:is (not (brick-on-ground-p (make-brick (list 0 0 10) (list 0 0 10)))))
  (fiveam:is (not (brick-on-ground-p (make-brick (list 0 0 0) (list 0 0 0)))))
  (fiveam:is (not (brick-on-ground-p (make-brick (list 0 0 0) (list 1 0 0)))))
  (fiveam:is (not (brick-on-ground-p (make-brick (list 1 1 0) (list 1 1 0)))))
  (fiveam:is (not (brick-on-ground-p (make-brick (list 0 1 0) (list 0 1 0)))))
  (fiveam:is (brick-on-ground-p (make-brick (list 0 0 1) (list 0 0 1))))
  (fiveam:is (brick-on-ground-p (make-brick (list 0 0 1) (list 0 0 5))))
  (fiveam:is (brick-on-ground-p (make-brick (list 4 7 5) (list 4 7 1)))))

(fiveam:run! 'test-brick-on-ground)
(fiveam:run! 'test-bricks)
