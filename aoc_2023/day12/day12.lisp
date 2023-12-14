(in-package :aoc_2023)

(ql:quickload :split-sequence)
(ql:quickload :cl-permutation)
(ql:quickload :cl-ppcre)
(ql:quickload :alexandria)
(ql:quickload :arrows)

(use-package :arrows)

(defparameter *day-number* 12)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-pathname (filename)
    (arrows:-> filename
               (merge-pathnames (format nil "day~d/inputs/" *day-number*))
               (merge-pathnames #p"/home/alex/src/workspace/advent_of_code/aoc_2023/"))))

(defparameter *input* (get-pathname "input.txt"))

(defun note (&rest args) )
(defun not-implemented (&optional description &rest body)
  "Placeholder function for bits of code that aren't implemented yet"
  (error (format nil "Not implemented: ~a"
                 (if description description "no description given"))))

(defparameter +operational-spring-char+ #\.)
(defparameter +damaged-spring-char+ #\#)
(defparameter +unknown-spring-char+ #\?)

(defparameter *whitespace-regex* (ppcre:create-scanner "\\s+"))
(defparameter *count-separator-regex* (ppcre:create-scanner "\\s*,\\s*"))
(defparameter *leading-damages-regex* (ppcre:create-scanner "^#+"))

(defparameter *operational-spring-regex* (ppcre:create-scanner "\\.+"))
(defparameter *damaged-spring-regex* (ppcre:create-scanner "#+"))
(defparameter *unknown-springs-regex* (ppcre:create-scanner "\\?+"))
(defparameter *unknown-spring-regex* (ppcre:create-scanner "\\?"))
(defparameter *spring-regex* (ppcre:create-scanner "[^\.]+"))

(defun unknown-components (spring)
  (ppcre:all-matches-as-strings *unknown-springs-regex* spring))

(defun operational-components (spring)
  (ppcre:all-matches-as-strings *operational-spring-regex* spring))

(defun damaged-components (spring)
  (ppcre:all-matches-as-strings *damaged-spring-regex* spring))

(defun arrangement-satisfies-counts-p (arrangement counts)
  (equal counts
         (mapcar #'length
                 (damaged-components arrangement))))

(defun permutations (lst &key (duplicate-p #'equal))
  (if (not (cdr lst))
      (list lst)
      (let ((others (loop for cursor from 0 below (length lst)
                          collect
                          (nconc (subseq lst cursor (+ 1 cursor))
                                 (subseq lst 0 cursor)
                                 (subseq lst (+ 1 cursor))))))
        (reduce #'nconc (remove-duplicates others :test duplicate-p)
                :key (lambda (init)
                       (mapcar (lambda (perm)
                                 (cons (car init) perm))
                               (permutations (cdr init))))))))

(defun list-find-all-positions (needle haystack)
  (loop for element in haystack
        and position from 0
        when (eql element needle)
          collect position))

(defun seq-find-all-positions (needle haystack)
  (loop for element across haystack
        and position from 0
        when (eql element needle)
          collect position))

(defun unaccounted-damaged-spring-bits (spring counts)
  (let* ((damaged (damaged-components spring))
         (damaged-count (reduce #'+ damaged :key #'length))
         (total-counts (lib:sum counts))
         (unaccounted-damaged-bits (- total-counts damaged-count)))
    unaccounted-damaged-bits))

(defun apply-unknown-map (map arrangement)
  (loop for char across arrangement
        and idx from 0
        collect
        (if (find idx map)
            +damaged-spring-char+
            char)))

(defun combine-bit-map-with-unknown-positions (bit-map unknown-positions)
  (remove nil
          (mapcar (lambda (b p) (if (= 0 b) NIL p))
                  bit-map
                  unknown-positions)))

(defun combine-bit-maps-with-unknown-positions (bit-maps unknown-positions)
  (->> bit-maps
       (mapcar (lambda (bmap)
                 (combine-bit-map-with-unknown-positions bmap
                                                         unknown-positions)))))

(defun arrangements (spring counts)
  (note "Only need to work in groups separated by operational bits.")
  (let* ((all-unknown-positions (seq-find-all-positions +unknown-spring-char+
                                                        spring))
         (unaccounted-damaged-total (unaccounted-damaged-spring-bits spring
                                                                     counts))

         (bit-map (loop for unknown-position in all-unknown-positions
                        and n from 0
                        collect
                        (if (< n unaccounted-damaged-total)
                            1
                            0)))
         (bit-maps (permutations bit-map))
         (unknowns-map (combine-bit-maps-with-unknown-positions bit-maps
                                                                all-unknown-positions)))
    (->> (mapcar (lambda (map) (apply-unknown-map map spring)) unknowns-map)
         (remove-if-not (lambda (arrangement)
                          (arrangement-satisfies-counts-p arrangement counts))))))


(defun arrangements-n (spring counts)
  (length
   (arrangements spring counts)))


(defun parse-counts-string (str)
  (mapcar #'parse-integer (ppcre:split *count-separator-regex* str)))

(defun parse-input-line (line)
       (destructuring-bind (spring counts-str)
           (ppcre:split *whitespace-regex* line)
         (list spring (parse-counts-string counts-str))))

(defun part1 (filename)
  (->> (lib:read-file-lines filename)
       (mapcar #'parse-input-line)))

(defun repeat (times lst)
  (let ((result '()))
    (dotimes (n times)
      (setf result (append lst result)))
    result))

(defun copies (times e)
  (let ((result '()))
    (dotimes (n times)
      (setf result (cons e result)))
    result))

(defun explode-input (input)
  (destructuring-bind (spring counts) input
    (list (format nil "~{~A~^?.~}" (copies 5 spring))
          (repeat 5   counts))))

(defun strip-leading-damages (spring counts)
  (let ((leading-damages (remove nil
                                 (multiple-value-bind (start end)
                                     (ppcre:scan *leading-damages-regex* spring)
                                   (list start end)))))
    (if (not leading-damages)
        (list spring counts)
        (destructuring-bind (start end) leading-damages
          (let ((new-first-count (- (car counts) end)))
            (list (subseq spring end)
                  (if (= 0 new-first-count)
                      (cdr counts)
                      (cons (-  (car counts) end)
                            (cdr counts)))))))))

;; (defmacro delay (form)
;;   `(lambda () ,form))

;; (defun force (thunk) (funcall thunk))

;; (defun get-left (tree)
;;   (force (cadr tree)))

;; (get-left (build-damaged-spring-search-tree "????.######..#####."))

;; (defmacro make-node (val left right)
;;   `(list ,val (delay ,left) (delay ,right)))

;; (defun build-damaged-spring-search-tree (spring)
;;   (format t "Next arg is: ~a" spring))

;; (defun build-damaged-spring-search-tree (spring)
;;   (multiple-value-bind (start end) (ppcre:scan *unknown-spring-regex* spring)
;;     (if (not start) ;; no more decision points
;;         (make-node spring NIL NIL)
;;         (make-node (subseq spring 0 start)

;;                    (build-damaged-spring-search-tree
;;                     (concatenate 'string "#" (subseq spring end)))

;;                    (build-damaged-spring-search-tree
;;                     (concatenate 'string "." (subseq spring end)))))))

;; (defun search-damaged-spring-tree (tree counts)
;;   )

(defun empty-p (str)
  (= 0 (length str)))

(defun walk-damaged-spring (spring counts previous-char-did-consume)
  (if (empty-p spring)
      spring
      (let ((current-char (elt spring 0)))
        (if (char= +unknown-spring-char+)
            ;; make choice and branch
            (if (char= +operational-spring-char+)
                (if (and previous-char-did-consume
                         (not (= 0 (car counts))))
                    NIL ;; invalid situation; we need to consume more via # chars
                    (walk-damaged-spring (subseq spring 1)
                                         counts
                                         NIL)
  (multiple-value-bind (start end) (ppcre:scan *unknown-spring-regex* spring)
    (if (not start) ;; no more decision points
        (arrangement-satisfies-counts-p spring counts)
        (let* ((everything-up-to-next-unknown (subseq spring 0 start))
               (everything-after-next-unknown (subseq spring (+ 1 start)))
               (new-counts (consume-counts everything-up-to-next-unknown counts)))
          (if (not new-counts)
              NIL ;; not a valid spring
              (walk-damaged-spring
               (concatenate 'string "#" everything-after-next-unknown)


(let ((result 0))
  (loop for input in (part1 (get-pathname "example.txt"))
        do
           (destructuring-bind (spring counts) input
             (setf result (+ result (arrangements-n spring counts)))))
  result)
