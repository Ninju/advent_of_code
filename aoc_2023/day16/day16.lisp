(in-package :aoc_2023)

(ql:quickload :split-sequence)
(ql:quickload :cl-ppcre)
(ql:quickload :alexandria)
(ql:quickload :arrows)

(use-package :arrows)

(defparameter *day-number* 16)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-inputs-pathname (filename)
    (arrows:-> filename
               (merge-pathnames (format nil "day~d/inputs/" *day-number*))
               (merge-pathnames #p"/home/alex/src/workspace/advent_of_code/aoc_2023/"))))

(defparameter *input* (get-inputs-pathname "input.txt"))

(defparameter +space-char+ #\.)

(defparameter +splitter-horizontal-char+ #\-)
(defparameter +splitter-vertical-char+ #\|)
(defparameter +mirror-upward-char+ #\/)
(defparameter +mirror-downward-char+ #\\)

(defparameter +splitter-horizontal-char+ #\-)
(defparameter +splitter-vertical-char+ #\|)
(defparameter +mirror-upward-char+ #\/)
(defparameter +mirror-downward-char+ #\\)

(defun tile-p (char) (not (space-p char)))
(defun space-p (char) (char= +space-char+ char))

(defun light-collides-with (from-direction tile-char)
  (cond
    ((and (or (equal *north* from-direction)
              (equal *south* from-direction))
          (char= tile-char +splitter-horizontal-char+)) (list *west* *east*))

    ((and (or (equal *west* from-direction)
              (equal *east* from-direction))
          (char= tile-char +splitter-vertical-char+)) (list *north* *south*))

    ;; Upward mirror
    ((and (equal *north* from-direction)
          (char= tile-char +mirror-upward-char+)) (list *east*))

    ((and (equal *east* from-direction)
          (char= tile-char +mirror-upward-char+)) (list *north*))

    ((and (equal *south* from-direction)
          (char= tile-char +mirror-upward-char+)) (list *west*))

    ((and (equal *west* from-direction)
          (char= tile-char +mirror-upward-char+)) (list *south*))

    ;; Downward mirror
    ((and (equal *north* from-direction)
          (char= tile-char +mirror-downward-char+)) (list *west*))

    ((and (equal *east* from-direction)
          (char= tile-char +mirror-downward-char+)) (list *south*))

    ((and (equal *south* from-direction)
          (char= tile-char +mirror-downward-char+)) (list *east*))

    ((and (equal *west* from-direction)
          (char= tile-char +mirror-downward-char+)) (list *north*))

    (t (list from-direction))))

(defun build-map (filename)
  (lib:lists->2d-array (lib:read-file-lines (get-inputs-pathname filename))))

(defclass photon (standard-object)
  ((direction :initarg :direction
              :accessor direction)
   (position :initarg :position
             :reader get-position
             :writer (setf set-position))))

(defun make-photon (direction initial-position)
  (make-instance 'photon :direction direction
                         :position initial-position))

(defun split-photon (photon directions)
  (progn
    (setf (direction photon) (car directions))
    (if (not (cdr directions))
        (list photon)

        (cons photon
              (mapcar (lambda (dir)
                        (make-photon dir (get-position photon)))
                      (cdr directions))))))

(defun step-photon (photon map)
  (let ((photon-direction (direction photon))
        (photon-position  (get-position photon)))
    (let* ((next-photon-position (add-points photon-direction
                                             photon-position)))

      (if (let ((*map* map))
            (out-of-bounds-p next-photon-position))
          NIL
          (let ((map-cell (get-map-pos map next-photon-position)))
            (setf (set-position photon) next-photon-position)

            (if (space-p map-cell)
                (list photon)
                (let ((photon-directions (light-collides-with photon-direction
                                                              map-cell)))
                  (split-photon photon photon-directions))))))))

(defun remove-out-of-bounds-light (photons map)
  (remove-if (lambda (p)
               (let ((*map* map)) (out-of-bounds-p (get-position p))))
             photons))

(defun step-map (map)
  (lambda (photons)
    (mapcan (lambda (p) (step-photon p map)) photons)))

(defun get-photons-on-tiles (new-photons map)
  "Returns the positions of photons that are on tiles"
  (let ((photon-positions (mapcar #'get-position new-photons)))
    (loop for pos in photon-positions
          when (tile-p (get-map-pos map pos))
          collect pos)))

(defun record-energized-tile (log pos)
  (setf (gethash pos log) 1))

(defun pretty-print-photon (photon)
  (format nil "~A heading ~A"
          (get-position photon)
          (direction photon)))

(defun main-iter (map photons energized-tiles-log)
  (let ((new-photons (mapcan (lambda (photon)
                               (step-photon photon map))
                             photons)))
    (let ((energized-tile-positions (mapcar #'get-position new-photons)))
      (dolist (pos energized-tile-positions)
        (record-energized-tile energized-tiles-log
                               pos)))
    new-photons))

(defun test-loop (testing)
  (loop
    for x = testing then (+ 1 x)
    do
       (print x)
       (if (> x 10)
           (return 09))))

(defun seen-by-loop-detector (loop-detector photon)
  (gethash (list (get-position photon) (direction photon))
           loop-detector))

(defun mark-seen-by-loop-detector (loop-detector photon)
  (setf (gethash (list (get-position photon) (direction photon))
           loop-detector)
        T))

(defparameter *max-loop-iterations* 1000000)

(defun main-loop (map initial-photons)
  (format t "~2&--- START: main-loop~%")
  (let ((energized-tiles-log (make-hash-table :size 10000 :test #'equal))
        (loop-detector (make-hash-table :size 100 :test #'equal)))
    (loop
      for photons = initial-photons then
                                    (remove-if
                                     (lambda (p)
                                       (seen-by-loop-detector loop-detector p))
                                     (main-iter map
                                                photons
                                                energized-tiles-log))
      and loop-count = 0 then (+ 1 loop-count)
      do
         (if (> loop-count *max-loop-iterations*)
             (return energized-tiles-log))

         (mapcar (lambda (p)
                   (mark-seen-by-loop-detector loop-detector p))
                 photons)

         (format t "~&~{~A~^ | ~}" (mapcar #'pretty-print-photon photons))
         (if (not photons)
             (return energized-tiles-log)))))

(defun count-energized-tiles (filename starting-position starting-direction)
  (let ((map (build-map filename)))
    (let ((initial-light (make-photon starting-direction
                                      starting-position)))

      (->> (main-loop map (list initial-light))
           (alexandria:hash-table-keys)
           (length)))))

(defun part1 (filename)
  (count-energized-tiles filename (make-pos -1 0) *east*))

(part1 "example.txt")

(part1 "input.txt")
