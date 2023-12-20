(in-package :aoc_2023)

(defparameter *north* '(0 -1))
(defparameter *south* '(0  1))
(defparameter *west*  '(-1  0))
(defparameter *east*  '(1 0))

(defun x-coord (pos) (destructuring-bind (x y) pos x))
(defun y-coord (pos) (destructuring-bind (x y) pos y))
(defun add-points (p1 p2)
  (list (+ (x-coord p1) (x-coord p2))
        (+ (y-coord p1) (y-coord p2))))

(defclass aoc-explorer (standard-object)
  ((location :initarg :location :accessor location)
   (target :initarg :target :reader target)
   (travel-directions :initarg :travel-directions :accessor travel-directions)
   (grid :initarg :grid :reader grid)
   (visited :reader visited)
   (unvisited :reader unvisited)
   (distances-log :reader distances-log)))

(defmethod initialize-instance :after ((explorer dijkstra-explorer) &key)
  (with-slots (location grid distances-log) explorer
      (setf (slot-value explorer 'visited) (mark-all-nodes-unvisited grid))
      (setf (slot-value explorer 'unvisited) (create-unvisited-set grid))
      (setf (slot-value explorer 'distances-log) (create-tentative-distances-log grid))

    (setf (aref distances-log (y-coord location) (x-coord location)) 0)))

(defgeneric find-unvisited-neighbours (explorer))
(defmethod find-unvisited-neighbours ((explorer dijkstra-explorer))
  (with-slots (location travel-directions unvisited) explorer
      (->> travel-directions
           (mapcar (lambda (d) (add-points location d)))
           (remove-if-not (lambda (p) (member p unvisited :test #'equal))))))

(defun remove-location-from-unvisited (location unvisited)
  (remove location unvisited :test #'equal))

(defgeneric step-forward-one (explorer))
(defmethod step-forward-one ((explorer dijkstra-explorer))
  (with-slots (location target travel-directions grid visited unvisited distances-log) explorer
    (let ((neighbours (find-unvisited-neighbours explorer)))
      (update-neighbour-distances grid location neighbours distances-log)

      (mark-location-as-visited location visited)

      (setf (slot-value explorer 'unvisited)
            (remove-location-from-unvisited location unvisited))

      (print unvisited)

      (setf (slot-value explorer 'location)
            (choose-lowest-distance-away-from-start
               (if neighbours neighbours unvisited)
                distances-log)))))

(defgeneric search-shortest-path (explorer))
(defmethod search-shortest-path ((explorer dijkstra-explorer))
  (with-slots (target visited unvisited distances-log) explorer
    (loop for n from 0 to 100 do
      (cond ((visited-p target visited) (return distances-log))
            ((not unvisited) (return distances-log))
            (t (step-forward-one explorer))))))

(defun visited-p (location visited)
  (aref visited (y-coord location) (x-coord location)))

(defun choose-lowest-distance-away-from-start (locations distances-log)
  (let ((min-seen NIL)
        (result NIL))

    (loop for location in locations
          do
             (let ((dist (aref distances-log
                               (y-coord location)
                               (x-coord location))))
               (if dist
                   (if (not min-seen)
                       (progn (setf min-seen dist)
                              (setf result location))
                       (if (< dist min-seen)
                           (progn (setf min-seen dist)
                                  (setf result location)))))))
    result))

(defun mark-location-as-visited (location visited)
  (setf (aref visited (y-coord location) (x-coord location))
        T))

(defun update-neighbour-distances (grid current-location neighbours distances-log)
  (loop for neighbour in neighbours
        do
           (let* ((prev-distance (aref distances-log
                                       (y-coord neighbour)
                                       (x-coord neighbour)))

                  (cur-distance (aref distances-log
                                      (y-coord current-location)
                                      (x-coord current-location)))

                  (travel-cost (aref grid
                                     (y-coord neighbour)
                                     (x-coord neighbour)))

                  (new-distance (+ travel-cost cur-distance)))

             (cond
               ((and prev-distance (<= prev-distance new-distance)) NIL)
               (t (setf (aref distances-log (y-coord neighbour) (x-coord neighbour))
                        new-distance))))))

(defun mark-all-nodes-unvisited (map)
  (let ((map-dims (array-dimensions map)))
    (make-array map-dims :initial-element NIL)))

(defun create-unvisited-set (map)
  (let* ((map-dims (array-dimensions map)))
    (destructuring-bind (h w) map-dims
      (loop for y from 0 below h
            appending
            (loop for x from 0 below w
                  collect
                  (list x y))))))

(defun create-tentative-distances-log (map)
  (let* ((map-dims (array-dimensions map)))
    (make-array map-dims :initial-element NIL)))

(defun start-search (current to map)
  (let ((explorer (make-instance 'dijkstra-explorer
                                 :location current
                                 :travel-directions (list *north*
                                                          *south*
                                                          *west*
                                                          *east*)
                                 :target to
                                 :grid map)))
    ))

#+nil
(create-unvisited-set
 (build-map (get-inputs-pathname "simple_test.txt")))

#+nil
(let ((explorer (make-instance 'dijkstra-explorer
                 :location '(0 0)
                 :travel-directions (list *north*
                                          *south*
                                          *west*
                                          *east*)
                 :target '(5 5)
                 :grid (build-map (get-inputs-pathname "simple_test.txt")))))
  (step-forward-one explorer)
  (location explorer))

#+nil
(let ((explorer (make-instance 'dijkstra-explorer
                 :location '(0 0)
                 :travel-directions (list *north*
                                          *south*
                                          *west*
                                          *east*)
                 :target '(12 12)
                 :grid (build-map (get-inputs-pathname "example.txt")))))
  (search-shortest-path explorer)
  (distances-log explorer))

#+nil
(let ((visited (mark-all-nodes-unvisited (build-map (get-inputs-pathname "simple_test.txt")))))
  (mark-location-as-visited '(1 5) visited)
  visited)

#+nil
(let ((unvisited (create-unvisited-set (build-map (get-inputs-pathname "simple_test.txt")))))
  (remove-location-from-unvisited '(0 0) unvisited)
  unvisited)
