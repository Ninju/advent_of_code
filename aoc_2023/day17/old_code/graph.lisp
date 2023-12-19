(in-package :aoc_2023)

(defparameter *edge-id* 1)

(defclass edge (standard-object)
  ((start-position :initarg :start-position :accessor start-position)
   (end-position :initarg :end-position :accessor end-position)
   (direction :initarg :direction :accessor direction)
   (cost :initarg :cost :accessor cost)
   (edge-id :initform (incf *edge-id*) :reader edge-id)))

;; Positions are unique according to the x,y and the direction it was approached
(defun make-graph-position (x y direction-of-approach)
  (list x y direction-of-approach))

(defun scale-pos (scalar pos)
  (destructuring-bind (x y) pos
    (make-pos (* x scalar)
              (* y scalar))))

(defun zip-positions (fn p1 p2)
  (destructuring-bind (x1 y1) p1
    (destructuring-bind (x2 y2) p2
      (mapcar fn
              (list x1 y1)
              (list x2 y2)))))

(defun x-coord (pos)
  (destructuring-bind (x y) pos
    x))

(defun y-coord (pos)
  (destructuring-bind (x y) pos
    y))

(defun next-n-positions (position direction n)
  (loop for k from 1 to n
        collect
        (add-points position (scale-pos k direction))))

(defun cost-of-path (path map)
  (loop for pos in path
        sum (get-map-pos map pos)))

(defun all-subpaths-in-range (start direction range)
  (let ((end-positions (next-n-positions start direction range)))
    (maplist #'identity
            (reverse end-positions))))

(defun out-of-bounds-edge-p (edge)
  (or (out-of-bounds-p (start-position edge))
      (out-of-bounds-p (end-position edge))))

(defun create-edge-from-path (start path-to-destination direction map)
  (let* ((total-cost (cost-of-path path-to-destination map)))
    (make-instance 'edge
                   :start-position start
                   :end-position (car path-to-destination) ;; ASSUME: path in reverse order
                   :cost total-cost
                   :direction direction)))

(defun create-edge (start direction path-length map)
  (let ((path-to-destination (next-n-positions start direction path-length)))
    (create-edge-from-path start path-to-destination direction map)))

(defun out-of-bounds-path-p (path) (some #'out-of-bounds-p path))

(defun build-list-of-edges (map moves-in-a-row)
  (destructuring-bind (h w) (array-dimensions map)
    (let ((*map* map))
      (loop for y from 0 below h
            appending
            (loop for x from 0 below w
                  appending
                  (loop for direction in *directions-of-travel*
                        appending
                        (let* ((current-pos (make-pos x y))
                               (paths (->> (all-subpaths-in-range current-pos
                                                                  direction
                                                                  moves-in-a-row)
                                           (remove-if #'out-of-bounds-path-p)))
                               (edges (loop for path in paths
                                            collect
                                            (create-edge-from-path current-pos
                                                                   path
                                                                   direction
                                                                   map))))
                          edges)))))))

;; (defun build-graph (map moves-in-a-row)
;;   (let ((graph (make-hash-table :size 100 :test #'equal)))
;;     (destructuring-bind (h w) (array-dimensions map)
;;       (let ((*map* map))
;;         (loop for y from 0 below h
;;               do
;;                  (loop for x from 0 below w
;;                        do
;;                           (loop for direction in *directions-of-travel*
;;                                 do
;;                                    (let* ((current-pos (make-pos x y))
;;                                           (paths (->> (all-subpaths-in-range current-pos
;;                                                                              direction
;;                                                                              moves-in-a-row)
;;                                                       (remove-if #'out-of-bounds-path-p)))
;;                                           (edges (loop for path in paths
;;                                                        collect
;;                                                        (create-edge-from-path current-pos
;;                                                                               path
;;                                                                               direction
;;                                                                               map))))

;;                                      (multiple-value-bind (current-edges exists) (gethash current-pos graph)
;;                                        (if (not exists)
;;                                            (setf (gethash current-pos graph)
;;                                                  edges)
;;                                            (setf (gethash current-pos graph)
;;                                                  (append edges current-edges))))))))))
;;     graph))
