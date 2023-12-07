(in-package :aoc_2023)

(defparameter example-input "/home/alex/src/workspace/advent_of_code/aoc_2023/day5/example_maps.txt")
(defparameter input "/home/alex/src/workspace/advent_of_code/aoc_2023/day5/input.txt")

(ql:quickload :alexandria)
(ql:quickload :cl-ppcre)
(ql:quickload :lparallel)

(defparameter *number-of-workers* 10)
(setf lparallel:*kernel* (lparallel:make-kernel *number-of-workers*))

(defun get-lines (filename)
  (lib:read-file-line-groups filename))

(defun get-seeds (fileinput)
    (car fileinput))

(defun get-maps (fileinput)
    (cdr fileinput))

(defun discard-headers (fileinput)
    (remove 'nil (mapcar #'cdr fileinput)))

(defun build-maps (fileinput-maps-only)
  (loop for group in fileinput-maps-only
        collect
        (mapcar #'lib:extract-numbers-from-line group)))

(defparameter example-map "50 98 2")

(defparameter *example-seeds* '(79 14 55 13))

(defparameter *input-seeds* '(1263068588 44436703 1116624626 2393304 2098781025 128251971 2946842531 102775703 2361566863 262106125 221434439 24088025 1368516778 69719147 3326254382 101094138 1576631370 357411492 3713929839 154258863))

(defparameter *seeds* *example-seeds*)

(defun source (map-entry)
  (cadr map-entry))

(defun range (map-entry)
  (caddr map-entry))

(defun seed-does-match (seed map-entry)
  (<= (source map-entry) seed (+ (source map-entry) (range map-entry))))

(defun value->location (value map-entry)
  (destructuring-bind (dest source range) map-entry
      (+ dest (- value source))))

(defun find-map-entries (initial-value map)
  (loop for map-entry in map
        when
        (<= (source map-entry)
            initial-value
            (+ (source map-entry) (range map-entry)))
        collect
        map-entry))

(defun find-location (value map)
  (let ((map-entries (find-map-entries value map)))
    (if map-entries
        (value->location value (car map-entries))
        NIL)))

(defun find-locations (initial-value maps)
  (cdr (reduce (lambda (vs map)
            (let ((loc (find-location (car vs) map)))
              (if loc
                  (cons loc (cons loc (cdr vs)))
                  (cons (car vs) vs))))
          maps
          :initial-value (list initial-value))))

(defun range-overlap (start-1 end-1 start-2 end-2)
  "Returns the unmatched left, overlap, and unmatched right"

  (let ((left (if (< start-1 start-2)
                  (if (< end-1 start-2)
                      (list start-1 end-1)
                      (if (= end-1 start-2)
                          (list start-1 (- start-2 1))))))

        (right (if (> end-1 end-2)
                   (if (= start-1 end-2)
                       (list (+ 1 start-1) end-1)
                       (list
                        (max start-1 end-2) end-1))))

        (overlap (if (< start-1 start-2)
                     (if (< start-2 end-1)
                         (list start-2 (min end-1 end-2))))))

    (list left overlap right)))

;; (defun match-ranges (range-start range-end list-of-ranges)
;;   "Returns the matched portion of the ranges and any unmatched areas"
;;   (reduce (lambda (matches current-range)
;;             (destructuring-bind (cur-start cur-end) current-range
;;               (destructuring-bind (unmatched . already-matched) matches
;;               (loop for match in unmatched
;;                     collect
;;                     (destructuring-bind (unmatched-start unmatched-end) unmatched
;;                       (< unmatched-start cur-start)

;;                       (> unmatched-end cur-end)

;;           :initial-value (cons (list (list range-start range-end)) '()))


(defun location-number (seed-number maps)
  (car (find-locations seed-number maps)))

(defun pairs (lst)
  (loop for idx from 0 below (length lst) by 2
        collect
        (list (elt lst idx)
              (elt lst (+ 1 idx)))))

(defun main (&optional (filename example-input))
  (let ((maps (build-maps
               (discard-headers
                (get-maps
                 (get-lines filename))))))
    (lparallel:preduce-partial
     (lambda (current-min seed-range)
       (let ((range-start (car seed-range))
             (range-length (cadr seed-range)))
           (progn
             ;; (format t "~&Processing seed (numero ~d) start: ~d..~d~%" (+ 1 idx) range-start (- (+ range-start range-length) 1))
             (loop for r from range-start to (- (+ range-length range-start) 1)
                   minimizing
                   (progn
                     ;; (format t "~&Processing seed value: ~d~%" r)
                     (let ((location (location-number r maps)))
                       (if current-min
                         (min current-min location)
                         location)))))))
     (pairs *seeds*)
     :parts 10
     :initial-value NIL)))
;;       (location-number r maps))
;;))))

;; OR (alexandria:iota <length> :start <start> :step <step>)
(defun gen-range (x0 x1) (loop for n from x0 to x1 collect n))

(defun generate-vals-from-pairs-of-range (xs)
  (loop for idx from 0 below (length xs) by 2
        appending
        (let ((range-start (elt xs idx))
              (range-length (elt xs (+ 1 idx))))
          (alexandria:iota range-length :start range-start :step 1))))

;; (let ((*seeds* *input-seeds*))
;;   (let ((answer (main input)))
;;     (format t "The P Answer IS: ~d~%" answer)
;;     (format t "~&THE ONSWER IS: ~d~%" (reduce #'min answer))
;;     (format nil "~d" answer)))

;; processing in parallel I got these minimal values given the different seeds (start + range length) :
;; #(2369814042 2324727144 427916783 199602917 2254687 811357077 1153910022
  ;; 1410417518 95285571 369762034))



;; 2254687 also incorrect ARGHHH!!!
;; 2254708 - Incorrect????
 ; TOO HIGH
  ; => 2258308 (22 bits, #x227584)
 ; => 2258308 (22 bits, #x227584)
 ; => 199602917 (28 bits, #xBE5B2E5)

(let ((*seeds* *input-seeds*)
      (filename input))
  (let ((maps (build-maps
               (discard-headers
                (get-maps
                 (get-lines filename))))))
    (let ((answer (loop for seed-info in (pairs *seeds*)
                        minimizing
                        (destructuring-bind (range-start range-length) seed-info
                          (loop for n from range-start below (+ range-start range-length)
                                minimizing
                                (location-number n maps))))))
      (format t "ANSWER Via the old code: ~d" answer)
      (format nil "ANSWER Via the old code: ~d" answer))))


 ; => 2254686 (22 bits, #x22675F)
