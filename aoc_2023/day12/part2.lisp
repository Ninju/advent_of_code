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


(defparameter *spring-arrangements* (make-hash-table :size 10000 :test #'equal))

(defparameter *max-node-id* 0)

(defclass lazy-node (standard-object)
  ((parent :initarg :parent :accessor parent)
   (value :initarg :value :accessor lazy-node-value)
   (id :initform (incf *max-node-id*) :reader lazy-node-id)
   (left :initarg :left :accessor left)
   (right :initarg :right :accessor right)))

(defmacro make-lazy-node (parent value left right)
  `(make-instance 'lazy-node
    :parent ,parent
    :value ,value
    :left (lazy ,left)
    :right (lazy ,right)))

(defun right-get (lazy-node) (some-> lazy-node (right) (lazy-value)))
(defun left-get (lazy-node) (some-> lazy-node (left) (lazy-value)))

(defun right-get-val (lazy-node) (some-> lazy-node (right-get) (lazy-node-value)))
(defun left-get-val (lazy-node) (some-> lazy-node (left-get) (lazy-node-value)))

(defun build-lazy-spring-tree (spring &optional parent)
  (if (= 0 (length spring))
      NIL
      (multiple-value-bind (start end) (ppcre:scan "\\?" spring)
        (if (not start)
            (make-lazy-node parent
                            spring
                            NIL
                            NIL)
            (make-lazy-node parent
                            (subseq spring 0 start)
                            (build-lazy-spring-tree (format nil
                                                            "#~A"
                                                            (subseq spring end)))
                            (build-lazy-spring-tree (format nil
                                                            ".~A"
                                                            (subseq spring end))))))))

(let ((SLYNK-STICKERS:*BREAK-ON-STICKERS* (list :after)))
  (lazy-string-paths (build-lazy-spring-tree "abc??def")))

(defclass count-consumption-result (standard-object)
  ((new-counts :initarg :counts :reader get-counts)
   (success :initarg :success :reader success-p)
   (remainder :initarg :remainder :reader get-remainder)
   (unconsumed-string-end :initarg :unconsumed-string-end :reader get-unconsumed-string-end)))

(defun consume-counts (spring counts)
  (make-instance 'count-consumption-result
                 :counts counts
                 :success t
                 :remainder 0
                 :unconsumed-string-end ""))

(defun record-result ()
  (setf (gethash *current* *spring-arrangements*) T))

(defun choose (char tree) (get-child-with-leading-char char tree))

(defun branch (tree counts) )

(defun spring-tree-satisfies-counts-p (tree counts)
  (cond
    ((and (not tree) counts)
      ;; failed to consume counts before reaching end of tree
      NIL)

    ((and (not tree) (not counts)) (record-result))

    (t (let ((spring-partial (node-value tree)))
        (with-slots (new-counts success remainder unconsumed-string-end)
            (consume-counts spring-partial counts)

          (cond
            ((not success)         NIL)

            ;; next char MUST consume
            ((> remainder 0) (spring-tree-satisfies-counts-p (choose #\# tree)
                                                             (cons remainder
                                                                   new-counts))

             ;; next char must NOT consume
             ((and (= remainder 0)
                   (not new-counts)) (spring-tree-satisfies-counts-p (choose #\. tree)
                                                                     new-counts))

             ;; next char must NOT consume
             ((and (= remainder 0)
                   (= (length unconsumed-string-end) 0))

              (spring-tree-satisfies-counts-p (choose #\. tree)
                                              new-counts))


             ;; there were chars at the end that didn't have damaged parts, so we can
             ;; ditch the remainder as we need to check both branches of the tree
             ((= remainder 0) (branch tree
                                      new-counts))

             ;; we check remainder is ZERO and remainder > ZERO, so this should never
             ;; happen
             (t (error "Remainder is a negative number?")))))))))

(defun const (a b) a)
(defun const-b (a b) a)

(defun lazy-node-remove-if (lazy-node predicate &optional acc-fn acc-init)
  (if (not acc-fn)
      (lazy-node-remove-if lazy-node predicate #'const-b NIL)
      (let* ((current-node-val (lazy-node-value lazy-node))
             (acc (funcall acc-fn acc-init current-node-val)))

        (if (funcall predicate acc)
            (make-lazy-node (parent lazy-node)
                            current-node-val
                            (lazy-node-remove-if (left-get lazy-node)
                                                 predicate
                                                 acc-fn
                                                 acc)
                            (lazy-node-remove-if (right-get lazy-node)
                                                 predicate
                                                 acc-fn
                                                 acc))))))

(defun lazy-node-paths (lazy-node)
  (if (not lazy-node)
      NIL
      (let ((current-node-val (lazy-node-value lazy-node))
            (lhs (left-get lazy-node))
            (rhs (right-get lazy-node)))

        (if (and (not lhs)
                 (not rhs))
            (list current-node-val)
            (mapcar (lambda (p)
                      (cons current-node-val p))
                    (nconc (lazy-node-paths lhs)
                           (lazy-node-paths rhs)))))))

(lazy-node-paths *test-tree*)
(lazy-node-paths (build-lazy-spring-tree "abc??def"))

(defparameter *test-tree* (make-lazy-node NIL
                                          "abc"
                                          (make-lazy-node NIL
                                                          "def"
                                                          NIL
                                                          (make-lazy-node NIL
                                                                          "xyz"
                                                                          NIL NIL))
                                          (make-lazy-node NIL
                                                          "zyx"
                                                          (make-lazy-node NIL
                                                                          "vuw"
                                                                          NIL NIL)
                                                          (make-lazy-node NIL
                                                                          "ten"
                                                                          NIL NIL))))

(defun lazy-string-paths (lazy-node)
  (lazy-node-paths lazy-node
                   (lambda (cur path)
                     (format nil "~A~A" cur path))))


(defun lazy-node-remove-if-not (lazy-node predicate)
  (lazy-node-remove-if lazy-node (lambda (&rest args)
                                   (not (funcall predicate args)))))
