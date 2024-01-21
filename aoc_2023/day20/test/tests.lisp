(in-package :aoc_2023)

(ql:quickload :fiveam)

(fiveam:def-suite test-conjunctions
  :description "Test conjunctions")

(fiveam:in-suite test-conjunctions)

(defparameter *conj-fixture* (make-instance 'conjunction
                                            :id "test-conjunction"
                                            :outputs '("output")
                                            :inputs '("a" "b")))

(defparameter *flip-flop-a* (make-instance 'flip-flop
                                           :id "a"
                                           :outputs '("test-conjunction")))

(defparameter *flip-flop-b* (make-instance 'flip-flop
                                           :id "b"
                                           :outputs '("test-conjunction")))

(defun send-dispatch-to (dispatch receiver)
  (destructuring-bind (sender-id pulse-val dest-id) dispatch
    (receive-pulse receiver sender-id pulse-val)))

(defun dispatch-pulse (dispatch)
  (destructuring-bind (sender-id pulse-val dest-id) dispatch
    pulse-val))

(fiveam:test test-conjunctions-with-single-flip-flop
             (progn
               (let ((conj-fixture  (make-instance 'conjunction
                                                   :id "test-conjunction"
                                                   :outputs '("output")
                                                   :inputs '("a" "b"))))

                 (receive-pulse *flip-flop-a* "broadcaster" (pulse :low))
                 (receive-pulse *flip-flop-b* "broadcaster" (pulse :low))

                 (send-dispatch-to (dispatch *flip-flop-a*) conj-fixture)

                 (fiveam:is (equal (dispatch-pulse (dispatch conj-fixture))
                                   (pulse :low))))))

(fiveam:run! 'test-conjunctions-with-single-flip-flop)
