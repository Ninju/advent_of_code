(in-package :aoc_2023)

(defun make-hash-table-for-array-positions (array)
  (make-hash-table :size (ceiling
                          (* 1.5 (array-total-size array)))
                   :test #'equal))

(defparameter *visited* NIL)

(defun visited-p (pos)
  (multiple-value-bind (_val exists) (gethash pos *visited*)
    exists))

(defun mark-visited! (pos)
  (setf (gethash pos *visited*) T))

(defun lowest-cost-neighbour (neighbours distances)
  (car (reduce (lambda (min-so-far cur)
                 (let ((distance (gethash cur distances)))
                   (destructuring-bind (pos . min-distance) min-so-far
                     (if (> min-distance distance)
                         (cons cur distance)
                         min-so-far))))
               (cdr neighbours)
               :initial-value (cons (car neighbours) (gethash (car neighbours)
                                                              distances)))))

(defun find-shortest-path (grid starting-pos)
  ;; STEP 1
  (let ((visited (make-hash-table-for-array-positions grid))
        (tentative-distances (make-hash-table-for-array-positions grid)))

    ;; STEP 2
    (setf (gethash starting-pos tentative-distances) 0)

    (start-shortest-path-search grid starting-pos visited tentative-distances)))

(find '(1 1) '((1 1) (3 4) (2 2)) :test #'equal)

(defun find-next-unvisited (visited distances)
  (let ((visited-positions (alexandria:hash-table-keys visited)))
    (destructuring-bind (h w) (array-dimensions *map*)
      (let ((unvisited (remove-if
                        (lambda (pos)
                          (find pos visited-positions :test #'equal))
                        (loop for y from 0 below h
                              appending
                              (loop for x from 0 below w
                                    collect
                                    (make-pos x y))))))
        (let ((result NIL)
              (min-so-far NIL))
          (loop for pos in unvisited
                do
                   (let ((dist (gethash pos distances)))
                     (if (not min-so-far)
                         (progn (setf min-so-far dist)
                                (setf result pos))
                         (if dist
                             (if (> min-so-far dist)
                                 (progn (setf min-so-far dist)
                                        (setf result pos)))))))
              result)))))

(defun start-shortest-path-search (grid current-pos visited tentative-distances)
  (let ((*map* grid)
        (*visited* visited))

    (let ((current-tentative-distance (gethash current-pos tentative-distances))
          (neighbours (remove-if #'visited-p (get-neighbours current-pos))))

    (if (not neighbours)
        (let ((next-unvisited (find-next-unvisited visited tentative-distances)))
           (if (not next-unvisited)
               tentative-distances
               (start-shortest-path-search grid
                                           next-unvisited
                                           visited
                                           tentative-distances))))

        (progn
          (loop for neighbour in neighbours
                collect

                (let* ((cost (get-map-pos grid neighbour))
                       (new-tentative-distance (+ cost
                                                  current-tentative-distance)))

                  ;; STEP 3
                  (multiple-value-bind (prev exists) (gethash neighbour tentative-distances)
                    (if exists
                        (if (> prev new-tentative-distance)
                            (setf (gethash neighbour tentative-distances) new-tentative-distance))
                        (setf (gethash neighbour tentative-distances) new-tentative-distance)))))

          ;; STEP 4
          (mark-visited! current-pos)

          (start-shortest-path-search grid
                                      (lowest-cost-neighbour neighbours tentative-distances)
                                      visited
                                      tentative-distances)))))

;; (trace find-shortest-path)
;; (gethash '(5 5) (find-shortest-path (build-map "simple_test.txt") '(0 0)))gR
