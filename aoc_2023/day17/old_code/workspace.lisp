(in-package :aoc_2023)

(defparameter +really-large-number+ 99999999)
(defparameter *directions-of-travel* (list *north*
                                           *east*
                                           *south*
                                           *west*))

(defparameter *visited* NIL)
(defparameter *unvisited* NIL)

(defun make-hash-table-for-positions (max-x max-y)
  (make-hash-table :size (ceiling (* 1.5 max-x max-y)) :test #'equal))

(defun hash (pos direction-of-approach)
  (list pos direction-of-approach))

(defun get-distance-to (edge distances-map)
  (or (gethash (end-position edge) distances-map)
      +really-large-number+))

(defun set-distance-to (edge distances-map new-distance)
  (setf (gethash (end-position edge) distances-map)
        new-distance))

(defun edge-neighbours (edge edges)
  (let ((edge-end (end-position edge))
        (edge-direction (direction edge)))
    (remove-if (lambda (candidate)
                 (or (not (equal edge-end
                                 (start-position candidate)))

                     (equal edge-direction
                            (direction candidate))

                     (equal (reverse-direction edge-direction)
                            (direction candidate))))
               edges)))

(defun build-graph (edges)
  (let ((graph (make-hash-table :size (ceiling (* 1.5 (length edges))))))

    (loop for edge in edges
          do
             (let ((neighbours (edge-neighbours edge edges)))
               (setf (gethash (edge-id edge) graph) neighbours)))
    graph))

(defun build-unvisited (edges)
  (let ((unvisited (make-hash-table :size (ceiling (* 1.5 (length edges))))))
    (loop for edge in edges
          do
             (setf (gethash (edge-id edge) unvisited) edge))
    unvisited))

(defun find-shortest-path-graph (edges starting-pos)
  ;; STEP 1
  (let* ((graph (build-graph edges))
         (unvisited (build-unvisited edges))
         (visited (make-hash-table :size (ceiling (* 1.5 (length edges)))))
         (tentative-distances (make-hash-table :size (ceiling (* 1.5 (length edges))) :test #'equal)))


    (let ((initial-edge (make-instance 'edge
                                       :start-position starting-pos
                                       :end-position starting-pos
                                       :cost 0

                                       ;; TODO: does the code work with NIL direction for the initial edge?
                                       :direction (list -1 -1))))

      (setf (gethash (edge-id initial-edge) graph)
            (edge-neighbours initial-edge edges))

      ;; STEP 2
      (set-distance-to initial-edge tentative-distances 0)
      (let ((*visited* visited)) (mark-visited! initial-edge))

      (start-shortest-path-graph-search initial-edge
                                        graph
                                        unvisited
                                        visited
                                        tentative-distances))))

;; TODO: visit edge or end position?
(defun mark-graph-visited! (edge)
  (setf (gethash (edge-id edge) *visited*) T))

(defun get-neighbouring-edges (source-edge graph)
  (gethash (edge-id source-edge) graph))

(defun edge-visited-p (edge) (gethash (edge-id edge) *visited*))

(defun lowest-cost-neighbour (neighbours tentative-distances)
  (car
   (reduce
    (lambda (min-so-far cur)
      (let ((distance (get-distance-to cur tentative-distances)))
        (destructuring-bind (pos . min-distance) min-so-far
          (if (> min-distance distance)
              (cons cur distance)
              min-so-far))))
    (cdr neighbours)
    :initial-value (cons (car neighbours) (get-distance-to (car neighbours) tentative-distances)))))

(defun remove-from-unvisited! (edge unvisited)
  (remhash (edge-id edge) unvisited))

(defun find-next-unvisited (distances unvisited graph)
  (let ((min-so-far NIL)
        (min-seen NIL))

    (loop for id in (alexandria:hash-table-keys unvisited)
          do
             (let ((cur (get-distance-to (gethash id unvisited) distances)))
               (cond
                 ((not min-so-far) (setf min-seen id))

                 ((> min-so-far cur) (setf min-seen id)))))

    (gethash min-seen unvisited)))

(defun start-shortest-path-graph-search (current-edge graph unvisited visited tentative-distances)
  (format t "~&+ Searching: ~a (came from: ~a)"
          (start-position current-edge) (direction current-edge))
  (let* ((*visited* visited)
         (neighbours (remove-if #'edge-visited-p
                                (get-neighbouring-edges current-edge graph))))

    (if (not neighbours)
        ;; TODO: should backtrack here to previously visited edges that have unvisited neighbours as we may have reached a dead-end
        (progn
            (remove-from-unvisited! current-edge unvisited)
            (mark-visited! current-edge)

            (let ((next-unvisited (find-next-unvisited tentative-distances unvisited graph)))
              (if (not next-unvisited)
                  (progn (format t "~&No next unvisited found")
                                 tentative-distances)

                  (progn (format t "~&- Backtracking to: ~a (-> ~a)"
                                 (start-position next-unvisited)
                                 (end-position next-unvisited))
                         (start-shortest-path-graph-search next-unvisited
                                                           graph
                                                           unvisited
                                                           visited
                                                           tentative-distances)))))

        (progn
          (let ((current-tentative-distance (get-distance-to current-edge tentative-distances)))
            (loop for neighbouring-edge in neighbours
                  do

                     (let* ((new-tentative-distance (+ (cost neighbouring-edge)
                                                       current-tentative-distance)))

                     ;; STEP 3
                       (multiple-value-bind (prev exists) (get-distance-to neighbouring-edge tentative-distances)
                         (if exists
                             (if (> prev new-tentative-distance)
                                 (set-distance-to neighbouring-edge tentative-distances new-tentative-distance))
                             (set-distance-to neighbouring-edge tentative-distances new-tentative-distance))))))

          ;; STEP 4
          (mark-graph-visited! current-edge)

          (let ((next-edge (lowest-cost-neighbour neighbours tentative-distances)))

            (remove-from-unvisited! current-edge unvisited)

            (start-shortest-path-graph-search next-edge
                                              graph
                                              unvisited
                                              visited
                                              tentative-distances))))
  tentative-distances))

(let ((edges (build-list-of-edges (build-map "example.txt") 1)))
  (let ((distances (find-shortest-path-graph edges '(0 0))))
    (->> (remove-if-not (lambda (edge) (equal (end-position edge) '(12 12)))
                        edges)
         (mapcar (lambda (edge) (get-distance-to edge distances))))))

(declaim (optimize (speed 0) (space 0) (debug 3)))
(defun factorial (n)
  (if (= 1 n)
      n
      (* n (factorial (- n 1)))))

;; (let ((edges (build-list-of-edges (build-map "example.txt") 1)))
;;   (->> (mapcar (lambda (edge)
;;             (list (start-position edge)
;;                   (end-position edge)))
;;           edges)
;;        (remove-if-not (lambda (edge)
;;                         (equal (cadr edge) '(12 12))))))
