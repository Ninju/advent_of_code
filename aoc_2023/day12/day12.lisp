(in-package :aoc_2023)

(ql:quickload :lazy)
(ql:quickload :split-sequence)
(ql:quickload :cl-permutation)
(ql:quickload :cl-ppcre)
(ql:quickload :alexandria)
(ql:quickload :arrows)

(use-package :arrows)
(use-package :lazy)

(defparameter *day-number* 12)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-inputs-pathname (filename)
    (arrows:-> filename
               (merge-pathnames (format nil "day~d/inputs/" *day-number*))
               (merge-pathnames #p"/home/alex/src/workspace/advent_of_code/aoc_2023/"))))

(defparameter *input* (get-inputs-pathname "input.txt"))

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
  (->> (lib:read-file-lines (get-inputs-pathname filename))
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

(defun get-left (tree)
  (lazy-value (cadr tree)))

(defun get-right (tree)
  (lazy-value (caddr tree)))

(defmacro make-node (val left right)
  `(list ,val (lazy ,left) (lazy ,right)))

(defun make-node-fn (val left right)
  (list val left right))


(defun build-damaged-spring-search-tree (spring)
  (multiple-value-bind (start end) (ppcre:scan *unknown-spring-regex* spring)
    (if (not start) ;; no more decision points
        (make-node spring NIL NIL)
        (make-node (subseq spring 0 start)

                   (build-damaged-spring-search-tree
                    (concatenate 'string "#" (subseq spring end)))

                   (build-damaged-spring-search-tree
                    (concatenate 'string "." (subseq spring end)))))))

(defparameter *rec* '())
(defun rec (x) (progn (push (node-value x) *rec*) x))

(setf *rec* '())
(->>
     (build-damaged-spring-search-tree "#?.?#..")
     (rec)
     (get-child :left)
     (rec)
     (get-child :left)
     (rec)
     (get-child :right)
     (rec))

(defun build-tree (input)
  (destructuring-bind (spring counts) input
      (list (build-damaged-spring-search-tree spring)
            counts)))

(defun get-child (child-id node)
  (cond
    ((eql child-id :left) (get-left node))
    ((eql child-id :right) (get-right node))))

(defun get-child-with-leading-char (c node)
  (cond
    ((char= c +damaged-spring-char+) (get-left node))
    ((char= c +operational-spring-char+) (get-right node))))

(define-condition invalid-arrangement-due-to-mismatched-counts (error)
  ((arrangement :initarg arrangement)
   (counts :initarg counts)))

(make-condition 'invalid-arrangement-due-to-mismatched-counts)

(defun consume-counts (spring counts)
  (if (not (ppcre:scan *damaged-spring-regex* spring))
      (values (list spring counts) 0 T)
      (let ((count (car counts)))

        (multiple-value-bind (start end) (ppcre:scan *damaged-spring-regex*
                                                     spring)

          (let* ((consumed (- end start))
                 (unconsumed-spring (subseq spring end)))
            (cond
              ;; we've consumed more than allowed (cluster of #s > count)
              ;; OR
              ;; we've not consumed the entire count and the following char
              ;; is not a #..
              ((or (> consumed count) (and (< consumed count)
                                           (> (length unconsumed-spring) 0)))

               (error 'invalid-arrangement-due-to-mismatched-counts
                      :counts counts
                      :arrangement spring))

              ((< consumed count) (values (list unconsumed-spring
                                                (cons (- count consumed)
                                                      (cdr counts)))
                                          consumed
                                          NIL))

              ((and (= consumed count)
                    (not (cdr counts))) (values (list unconsumed-spring NIL)
                                                consumed
                                                T))

              ((= consumed count) (consume-counts unconsumed-spring
                                                  (cdr counts)))))))))

(defun node-value (node)
  (car node))

(define-condition result-found (condition)
  ((result :initarg :result
           :reader get-result)))

(make-condition 'result-found)

(defun contains-damaged-p (spring)
  (ppcre:scan *damaged-spring-regex* spring))

;; Choose a branch and search that. Catch results, then float the result up to
;; top-level using `signal` and also branch right so we can search that side of
;; the tree
(defun make-decision (node counts current-arrangement)
  (let ((decisions (list +damaged-spring-char+)))

    (format t "~&++++ Making decision to go to '#'")
    (handler-case
        (branch-into +damaged-spring-char+
                     node
                     counts
                     current-arrangement)

      (invalid-arrangement-due-to-mismatched-counts ()
        (progn (format t "~&---- Stepping back to decision point (invalid counts)")

               (if (not (find +operational-spring-char+ decisions))
                   (progn (setf decisions (cons +operational-spring-char+ decisions))
                          (branch-into +operational-spring-char+
                                       node
                                       counts
                                       current-arrangement)))))

      (result-found (c) (progn
                          (format t "~&---- Stepping back to decision point: result found = ~S" (get-result c))
                          (signal c)
                          (if (not (find +operational-spring-char+ decisions))
                              (progn
                                     (setf decisions (cons +operational-spring-char+ decisions))
                                     (branch-into +operational-spring-char+
                                                  node
                                                  counts
                                                  current-arrangement))
                              (format t "~&---- ALL DECISIONS MADE")))))))

(defun mid-level
    ()
  (handler-bind ((result-found (lambda (c)
                                 (print (get-result c)))))
    (loop for n from 0 upto 100
          do
             (if (= 0 (mod n 7))
                 (progn (signal 'result-found :result n))))))

(defun top-level ()

  (let ((results '()))
    (handler-bind ((result-found (lambda (c)
                                   (push (get-result c) results))))
      (mid-level))
    results))

(top-level)

(defun branch-into (char node counts current-arrangement)
  (let ((child (get-child-with-leading-char char node))
        (new-arrangement (format nil
                                 "~A~A"
                                 current-arrangement
                                 (node-value node))))
    (if (not child)
        (if (not counts)
            (signal 'result-found :result new-arrangement)
            NIL)
        (progn (step-through-node child
                           counts
                           new-arrangement)
               (format t "~&    stepping through into child: ~c" char)))))

(defun step-through-node (node counts current-arrangement)
  (format t "~&'~a|~a' ~a" current-arrangement (node-value node) counts)

  (let ((current-value (node-value node)))
    (cond
      ((not counts) (if (contains-damaged-p current-value)
                        NIL
                        (branch-into +operational-spring-char+
                                     node
                                     counts
                                     current-arrangement)))

      ((not
        (contains-damaged-p current-value)) (make-decision node
                                                           counts
                                                           current-arrangement))

      (t (handler-case
             (multiple-value-bind
                   (result num-consumed fully-consumed) (consume-counts current-value
                                                                        counts)
               (destructuring-bind (unconsumed-spring new-counts) result

                   (cond
                     ((string= "" unconsumed-spring) (if fully-consumed
                                                          (branch-into +operational-spring-char+
                                                                       node
                                                                       new-counts
                                                                       current-arrangement)
                                                          (branch-into +damaged-spring-char+
                                                              node
                                                              new-counts
                                                              current-arrangement)))


                     (fully-consumed (make-decision node
                                                    new-counts
                                                    current-arrangement))

                     ((not fully-consumed)       (error "consume counts should have raised an error")))))

           ;; Condition handlers
           (invalid-arrangement-due-to-mismatched-counts () NIL))))))

(defun search-all-arrangements (root-node counts)
  (format t "~&Searching root-node ~A with counts ~a~%" root-node counts)

  (let ((arrangements '()))
    (handler-bind ((result-found (lambda (c3)
                                   (push (get-result c3) arrangements))))
      (step-through-node root-node counts ""))
    arrangements))

(defun arrangements-for-input (input)
  (destructuring-bind (tree counts) input
    (search-all-arrangements tree counts)))

 ; => ("???.###" (1 1 3))

;; (let ((arrangements (->> (part1 "example.txt")
;;                          ;; (mapcar #'explode-input)
;;                          (mapcar #'build-tree)
;;                          (mapcar #'arrangements-for-input))))
;;   arrangements)

;; (search-all-arrangements (build-damaged-spring-search-tree "?") '(1)) ;; '("..")

;; (step-through-node (build-damaged-spring-search-tree "?") '(1) "")

(search-all-arrangements (build-damaged-spring-search-tree "??????") '(1 1)) ;; '("..")

;; (node-value (build-damaged-spring-search-tree "?")

;; (defparameter *decisions* (make-hash-table :size 10000
;;                                            :test #'equal))

;; (defun search (string cursor decisions counts consuming-count current-arrangement)
;;   (let ((current-char (elt string cursor)))
;;     (cond
;;       (
;;        ;; Just finished consuming the count
;;        (and (char= current-char +unknown-spring-char+)
;;             (= 0 consuming-count))
;; (if (and consuming-count
;;                                                            (> consuming-count 0))
;;                                                       (progn (push (elt decisions cursor)
;;                                                                    +damaged-spring-char+)
;;                                                              (search string
;;                                                                      cursor
;;                                                                      decisions
;;                                                                      counts
;;                                                                      (- 1 consuming-count)
;;                                                                      (format nil "~A~A" current-arrangement +damaged-spring-char+)))
