(in-package :aoc_2023/day24)

(def-suite day24-test-suite)
(in-suite day24-test-suite)

(test test-cross-join
  (is (= (cross-join '())
         NIL))

  (is (= (cross-join '(1))
         NIL))

  (is (= (cross-join '(1 2))
         '((1 2))))

  (is (= (cross-join '(1 2 3))
         '((1 2) (1 3) (2 3))))

  (is (= (cross-join '(1 2 3 4))
         '((1 2) (1 3) (1 4) (2 3) (2 4) (3 4))))

  (is (= (cross-join '(1 2 3 4 5))
         '((1 2) (1 3) (1 4) (1 5)
           (2 3) (2 4) (2 5)
           (3 4) (3 5)
           (4 5))))

  (is (= (cross-join '(1 2 3 4 5 6))
         '((1 2) (1 3) (1 4) (1 5) (1 6)
           (2 3) (2 4) (2 5) (2 6)
           (3 4) (3 5) (3 6)
           (4 5) (4 6)
           (5 6))))

  (is (= 10
         (length (cross-join '(("19, 13, 30 @ -2,  1, -2")
                               ("18, 19, 22 @ -1, -1, -2")
                               ("20, 25, 34 @ -2, -2, -4")
                               ("12, 31, 28 @ -1, -2, -1")
                               ("20, 19, 15 @  1, -5, -3"))))))

  (let ((results (cross-join '(("19, 13, 30 @ -2,  1, -2")
                               ("18, 19, 22 @ -1, -1, -2")
                               ("20, 25, 34 @ -2, -2, -4")
                               ("12, 31, 28 @ -1, -2, -1")
                               ("20, 19, 15 @  1, -5, -3")))))
    (is (= results (remove-duplicates results :test #'equal)))))

(run! 'test-cross-join)

(load-data-from-file "example.txt")

(defun vec2= (v1 v2 &key (tolerance 0.001))
  (and (< (abs (- (vx v2) (vx v1))) tolerance)
       (< (abs (- (vy v2) (vy v1))) tolerance)))

(test test-ray-find-intersection-on-examples
  (is (vec2= (ray-find-intersection (parse-line "19, 13, 30 @ -2,  1, -2")
                                    (parse-line "18, 19, 22 @ -1, -1, -2"))

             (vec2 14.333 15.333)))

  (is (vec2= (ray-find-intersection (parse-line "19, 13, 30 @ -2, 1, -2")
                                    (parse-line "20, 25, 34 @ -2, -2, -4"))

             (vec2 11.667 16.667)))

  (is (vec2= (ray-find-intersection (parse-line "19, 13, 30 @ -2, 1, -2")
                                    (parse-line "12, 31, 28 @ -1, -2, -1"))
             (vec2 6.2 19.4)))

  (is-false (ray-find-intersection (parse-line "19, 13, 30 @ -2, 1, -2")
                                   (parse-line "20, 19, 15 @ 1, -5, -3"))
            "Hailstones' paths crossed in the past for hailstone A.")

  (is-false (ray-find-intersection (parse-line "18, 19, 22 @ -1, -1, -2")
                                   (parse-line "20, 25, 34 @ -2, -2, -4"))
            "Hailstones' paths are parallel; they never intersect.")

  (is (vec2= (ray-find-intersection (parse-line "18, 19, 22 @ -1, -1, -2")
                                    (parse-line "12, 31, 28 @ -1, -2, -1"))
             (vec2 -6 -5)))

  (is-false (ray-find-intersection (parse-line "18, 19, 22 @ -1, -1, -2")
                                   (parse-line "20, 19, 15 @ 1, -5, -3"))
            "Hailstones' paths crossed in the past for both hailstones.")

  (is (vec2= (ray-find-intersection (parse-line "20, 25, 34 @ -2, -2, -4")
                                    (parse-line "12, 31, 28 @ -1, -2, -1"))
             (vec2 -2 3)))

  (is-false (ray-find-intersection (parse-line "20, 25, 34 @ -2, -2, -4")
                                   (parse-line "20, 19, 15 @ 1, -5, -3"))
            "Hailstones' paths crossed in the past for hailstone B.")

  (is-false (ray-find-intersection (parse-line "12, 31, 28 @ -1, -2, -1")
                                   (parse-line "20, 19, 15 @ 1, -5, -3"))
            "Hailstones' paths crossed in the past for both hailstones."))

(let ((fiveam:*on-error* :debug))
  (fiveam:run! 'test-ray-find-intersection-on-examples))

#|
(defun matrix-scalar-multiply (scalar matrix)
  (destructuring-bind (h w) (array-dimensions matrix)
    (let ((results (make-array (list h w))))
    (loop for j from 0 below h
          do
          (loop for i from 0 below w
                do
                (setf (aref results j i)
                      (* scalar (aref matrix j i)))))
      results)))
|#

(test test-ray-find-intersection-on-input
  (is-false (ray-find-intersection (parse-line "377538583638132, 384072177674294, 493210474820043 @ -165, -234, -389")
                                   (parse-line "290713522571573, 269090190799220, 254214928464400 @ -99, 42, -32"))))

(let ((fiveam:*on-error* :debug))
  (fiveam:run! 'test-ray-find-intersection-on-input))

(test test-rays-parallel-p
  (is (rays-parallel-p (parse-line "18, 19, 22 @ -1, -1, -2")
                       (parse-line "20, 25, 34 @ -2, -2, -4")))

  (is-false (rays-parallel-p (parse-line "18, 19, 22 @ -1, -1, -2")
                             (parse-line "12, 31, 28 @ -1, -2, -1")))

  (is-false (rays-parallel-p (make-ray '((0 0) (1 -1)))
                             (make-ray '((0 0) (-1 1))))))


(run! 'test-rays-parallel-p)
