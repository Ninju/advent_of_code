(in-package :aoc_2023)

(ql:quickload :alexandria)
(ql:quickload :arrows)
(ql:quickload :cl-ppcre)

(use-package :arrows)

(declaim (optimize (debug 3)))

(defparameter *day-number* 19)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-inputs-pathname (filename)
    (-> filename
        (merge-pathnames (format nil "day~d/inputs/" *day-number*))
        (merge-pathnames #p"/home/alex/src/workspace/advent_of_code/aoc_2023/"))))

(defun alist-update (key alist new-value &rest args)
  (let ((args-with-key (append args (list :key #'car))))
    (funcall #'acons key new-value
             (apply #'remove key alist args-with-key))))

;; DATA STRUCTURES

(defun make-range (a b) (cons a b))
(defun start (range) (car range))
(defun end (range) (cdr range))

(defclass rule (standard-object)
  ((category :initarg :category :reader category)
   (operator :initarg :operator :reader operator)
   (threshold :initarg :threshold :reader threshold)
   (outcome :initarg :outcome :reader outcome)))

(defclass workflow (standard-object)
  ((id :initarg :id :reader id)
   (rules :initarg :rules :reader rules)
   (default-outcome :initarg :default-outcome :reader default-outcome)))

(defclass rating-part (standard-object)
  ((category :initarg :category :reader category)
   (value :initarg :value :reader value)))

(defclass rating (standard-object)
  ((parts :initarg :parts :reader parts)))

(defun make-workflow (id rules default)
  (make-instance 'workflow :id id
                           :rules rules
                           :default-outcome default))

(defun make-rating (parts)
  (make-instance 'rating :parts parts))

(defun make-rating-part (category value)
  (make-instance 'rating-part :category category :value value))

(defun make-rule (category operator threshold outcome)
  (make-instance 'rule :category category
                       :operator operator
                       :threshold threshold
                       :outcome outcome))

(defun get-rating-value (part-category rating)
  (some-> (find-if (lambda (p) (string= part-category (category p)))
                   (parts rating))
          (value)))

;; END DATA STRUCTURES
;; DYNAMIC BINDINGS

(defparameter *upper-limit* 4000)
(defparameter *initial-ranges* (list
                                (cons "x" (make-range 1 *upper-limit*))
                                (cons "m" (make-range 1 *upper-limit*))
                                (cons "a" (make-range 1 *upper-limit*))
                                (cons "s" (make-range 1 *upper-limit*))))

;; END DYNAMIC BINDINGS
;; PARSING

(defun parse-rating (rating)
  (let ((components (loop for match in (ppcre:all-matches-as-strings
                                        "[xmas]=([0-9]+)"
                                        rating)
                          collect
                          (destructuring-bind (cat val) (ppcre:split "=" match)
                            (make-rating-part cat (parse-integer val))))))
    (make-rating components)))


(defun parse-operator (op) op)

(defun lookup-operator (op)
  (cond
    ((string= op "<") #'<)
    ((string= op ">") #'>)
    (t (error (format nil "Operator cannot be found: ~a" op)))))

(defun parse-rule (rule)
  (ppcre:register-groups-bind (category operator threshold outcome)
      ("([xmas])(<|>)([0-9]+):([a-zA-Z]+)" rule)
    (make-rule category
               (parse-operator operator)
               (parse-integer threshold)
               outcome)))

(defun parse-workflow (string)
  (ppcre:register-groups-bind (name rules) ("([a-z]+){([^}]+)}" string)
    (let ((rule-parts (ppcre:split "," rules)))
      (make-workflow name
                     (mapcar #'parse-rule (butlast rule-parts))
                     (car (last rule-parts))))))

(defun load-file (filename)
  (destructuring-bind (workflows ratings) (lib:read-file-line-groups
                                           (get-inputs-pathname filename))
    (list (mapcar #'parse-workflow workflows) (mapcar #'parse-rating ratings))))

;; END PARSING
;; BUSINESS LOGIC

(defun apply-rule (rule rating)
  (let ((rating-value (get-rating-value (category rule) rating)))
    (if (funcall (lookup-operator (operator rule))
                 rating-value
                 (threshold rule))
        (outcome rule))))

(defun process-rating-through-workflow (rating workflow)
  (loop for rule in (rules workflow)
        do
           (let ((outcome (apply-rule rule rating)))
             (if outcome
                 (return-from process-rating-through-workflow outcome))))
  (default-outcome workflow))

(defun accepted-p (result)
  (string= "A" result))

(defun rejected-p (result)
  (string= "R" result))

(defun workflow-complete-p (result)
  (or (accepted-p result) (rejected-p result)))

(defun main-loop (workflows-hashmap rating)
  (let* ((entrypoint (gethash "in" workflows-hashmap))
         (workflow entrypoint))

    (loop
      (let ((interim-result
              (process-rating-through-workflow rating workflow)))
        (if (workflow-complete-p interim-result)
            (return-from main-loop interim-result)
            (setf workflow (gethash interim-result workflows-hashmap)))))))

(defun build-workflow-hashmap (workflows)
  (let ((hashmap (make-hash-table :size (* 2 (length workflows)) :test #'equal)))
    (loop for workflow in workflows
          do
             (setf (gethash (id workflow) hashmap) workflow))
    hashmap))

(defun find-accepted-ratings (workflows-hashmap ratings)
  (remove-if-not #'accepted-p
                 ratings
                 :key (lambda (r)
                        (main-loop workflows-hashmap r))))

(defun sum-rating-parts (rating)
  (reduce #'+ (parts rating) :key #'value))

(defun part1 (filename)
  (destructuring-bind (workflows ratings) (load-file filename)
    (let ((workflows-hashmap (build-workflow-hashmap workflows)))
      (reduce #'+
              (find-accepted-ratings workflows-hashmap ratings)
              :key #'sum-rating-parts))))

(defun restrict-range-to-meet-threshold (current-range operator threshold)
  (cond
    ((string= operator ">")
     (if (> (end current-range) threshold)
         (make-range (max (start current-range)
                          (+ threshold 1))
                     (max (end current-range)
                          (+ threshold 1)))))

    ((string= operator "<")

     (if (< (start current-range) threshold)
         (make-range (min (start current-range)
                          (- threshold 1))
                     (min (end current-range)
                          (- threshold 1)))))))

(defun range-for-rule (rule)
  (cond
    ((string= (operator rule) ">") (make-range (+ 1 (threshold rule))
                                               *upper-limit*))

    ((string= (operator rule) "<") (make-range 0
                                               (- (threshold rule)
                                                  1)))))

;; RUNE
;;
 ; => (1549 . 4000), (4001 . 4000), (838 . 1548)
(defun stable-range-intersect (range-1 range-2)
  (cond
    ((or (> (start range-1) (end range-2))
         (> (start range-2) (end range-1))) (values NIL range-1 range-2))

    (t
     (let* ((intersection (make-range (max (start range-1)
                                           (start range-2))
                                      (min (end range-1)
                                           (end range-2))))

            (less-than-range (make-range (min (start range-1)
                                              (start range-2))
                                         (- (max (start range-1)
                                                 (start range-2))
                                            1)))
            (more-than-range (if (not (= (end intersection)
                                         (end range-1)))
                                 (make-range (+ 1 (min (end range-1)
                                                       (end range-2)))
                                             (max (end range-1)
                                                  (end range-2))))))
       (values-list
        (cons intersection
              (if (< (start range-1) (start range-2))
                  (list less-than-range more-than-range)
                  (list more-than-range less-than-range))))))))

(defun apply-rule-to-ranges (rule ranges)
  (let* ((cat (category rule))
         (cat-range (cdr (assoc cat ranges :test #'equal))))

    (multiple-value-bind (matched-range lhs-unmatched rhs-unmatched)
        (range-intersect (range-for-rule rule) cat-range)

      (cons (if matched-range
                (cons (outcome rule)
                      (alist-update cat ranges matched-range :test #'equal)))

            (mapcar (lambda (unmatched-range)
                      (cons NIL (alist-update cat ranges unmatched-range :test #'equal)))
                    (remove nil (list lhs-unmatched rhs-unmatched)))))))


(defun apply-workflow-to-ranges (workflow ranges)
  (let ((results '())
        (unmatched-ranges (list ranges))
        (interim-ranges '()))

    (loop for rule in (rules workflow)
          do
             ;; (format t "~3&Processing next rule: ~A ~A ~A => ~A"
             ;;         (category rule)
             ;;         (operator rule)
             ;;         (threshold rule)
             ;;         (outcome rule))
             (loop for range in unmatched-ranges
                   do
                   (destructuring-bind (matched &rest unmatched)
                       (apply-rule-to-ranges rule range)

                     (if matched
                         (push matched results))

                     (if unmatched
                         (setf interim-ranges
                               (remove nil
                                       (append (mapcar #'cdr unmatched)
                                               interim-ranges))))))

             ;; (format t "~&UNMATCH RANGES = ~A" unmatched-ranges)
             ;; (format t "~&INTERIM RANGES = ~A" interim-ranges)
             (setf unmatched-ranges interim-ranges)
             (setf interim-ranges '())

             (append results matched)
             ;; (format t "~&INTERIM RANGES = ~A" interim-ranges)
             )



    (append results (mapcar (lambda (unmatched-range)
                                       (cons (default-outcome workflow)
                                             unmatched-range))
                                     unmatched-ranges))))


(defun load-workflows-hashmap-from-file (filename)
  (destructuring-bind (workflows ratings) (load-file filename)
    (declare (ignore ratings))
    (build-workflow-hashmap workflows)))

(defun step-forward-workflow (entrypoint workflows-hashmap initial-ranges)
  (let* ((workflow (gethash entrypoint workflows-hashmap))
         (ranges initial-ranges)
         (accepted-ranges '()))

    (let ((new-ranges (apply-workflow-to-ranges workflow ranges)))

      (let ((a-ranges (remove-if-not (lambda (e)
                                       (string= "A" (car e)))
                                     new-ranges))
            (other-ranges (remove-if (lambda (e)
                                       (or (string= "R" (car e))
                                           (string= "A" (car e))))
                                     new-ranges)))

        (setf accepted-ranges (append (mapcar #'cdr a-ranges)
                                      accepted-ranges))

        (list accepted-ranges other-ranges)))))

(defparameter *iter-limit* 10000)

(defun step-forward-workflows-recursively (workflows-hashmap
                                           queue
                                           acc-accepted)
  (if (not queue)
      (return-from step-forward-workflows-recursively acc-accepted))

  (decf *iter-limit*)
  (if (< *iter-limit* 0)
      (progn (setf *iter-limit* 0)
             (error "Reached iteration limit!")))

  (let* ((current (car queue))
         (current-workflow-id (car current))
         (current-ranges (cdr current)))

    (destructuring-bind (accepted others)
        (step-forward-workflow current-workflow-id
                               workflows-hashmap
                               current-ranges)

      (step-forward-workflows-recursively workflows-hashmap
                                          (append others (cdr queue))
                                          (append accepted acc-accepted)))))

(defun ranges->distinct-possibilities (ranges)
  (reduce #'*
          ranges
          :key (lambda (range) (- (end range) (start range)))))

(defun condense-range (r1 r2)
  (range-intersect '(300 . 450) '(200 . 300))
  )

(defun range-size (r) (- (end r) (start r)))

(defun consolidate-ranges (r1 r2)
  (loop for c in (list "x" "m" "a" "s")
        collect
        (multiple-value-bind (inter lhs rhs) (range-intersect (alexandria:assoc-value r1 c :test #'equal)
                                                              (alexandria:assoc-value r2 c :test #'equal))
            (if (not inter)
                (return nil)
                (cons c (list inter lhs rhs)))))))))

#+nil
(consolidate-ranges (caddr *test-accepted-ranges*)
                    (cadddr *test-accepted-ranges*))



(defun part2 ()
  (let ((all-m (loop for accepted in *test-accepted-ranges*
                     collect (assoc "s" accepted :test #'equal))))

    (remove-duplicates
     (sort (mapcar #'cdr all-m)
           #'>
           :key #'range-size) :test #'equal))
  ;;
  ;; (* 4000 4000 3711 4000)
  ;; (< (* 1337 838 1716 551) 167409079868000)
  ;; 167409079868000
  )

#+nil
(defparameter *test-rule* (parse-rule "s<537:gd,x>2440:R,A"))

#+nil
(defparameter *test-accepted-ranges* '((("m" 1549 . 4000) ("s" 2771 . 3448) ("x" 0 . 4000) ("a" 0 . 4000))
                                       (("m" 0 . 1548) ("s" 2771 . 3448) ("x" 0 . 4000) ("a" 0 . 4000))
                                       (("s" 3449 . 4000) ("x" 0 . 4000) ("m" 0 . 4000) ("a" 0 . 4000))
                                       (("a" 0 . 1716) ("m" 1801 . 4000) ("s" 1351 . 2770) ("x" 0 . 4000))
                                       (("a" 0 . 1716) ("m" 0 . 838) ("s" 1351 . 2770) ("x" 0 . 4000))
                                       (("m" 839 . 1800) ("s" 1351 . 2770) ("x" 0 . 4000) ("a" 0 . 4000))
                                       (("x" 0 . 2440) ("s" 537 . 1350) ("m" 0 . 2090) ("a" 2006 . 4000))
                                       (("x" 2663 . 4000) ("a" 0 . 2005) ("s" 0 . 1350) ("m" 0 . 4000))
                                       (("x" 0 . 1415) ("a" 0 . 2005) ("s" 0 . 1350) ("m" 0 . 4000))
                                       (("m" 2091 . 4000) ("a" 2006 . 4000) ("s" 0 . 1350) ("x" 0 . 4000))))

#+nil
(loop for ranges in (step-forward-workflows-recursively (load-workflows-hashmap-from-file "example.txt")
                                                        (list (cons "in" *initial-ranges*))
                                                        NIL)
      sum
      (ranges->distinct-possibilities (mapcar #'cdr ranges)))

 ; => 188598466743200 (48 bits, #xAB87809D8BA0)


#+nil
(step-forward-workflows-recursively (load-workflows-hashmap-from-file "example.txt")
                                    (list (cons "in" *initial-ranges*))
                                    NIL)

#+nil
(apply-rule-to-ranges *test-rule* *initial-ranges*)

#+nil
(car (mapcar #'cdr '(("gd" ("s" 0 . 536) ("x" 0 . 4000) ("m" 0 . 4000) ("a" 0 . 4000))
 (NIL ("s" 537 . 4000) ("x" 0 . 4000) ("m" 0 . 4000) ("a" 0 . 4000)))))

#+nil
(apply-workflow-to-ranges (parse-workflow "px{a<2006:qkq,m>2090:A,rfg}")
                          *initial-ranges*)

#+nil
(let ((workflows-hashmap (load-workflows-hashmap-from-file "example.txt")))
  (destructuring-bind (accepted others)
      (step-forward-workflow "in" workflows-hashmap
                             *initial-ranges*)
    (loop for other in others
          appending
          (step-forward-workflow (car other)
                                 (load-workflows-hashmap-from-file "example.txt")
                                 (cdr other)))))


#+nil
(assoc (category *test-rule*) *initial-ranges* :test #'equal)

#+nil
(range-intersect '(100 . 400) '(50 . 380))
;; => (50 . 99) (100 . 380) (381 . 400)

#+nil
(range-intersect '(0 . 4000) '(0 . 2005))

#+nil
(range-intersect '(0 . 1000) '(2000 . 2005))

#+nil
(restrict-range-to-meet-threshold '(0 . 4000) "<" 2006)

#+nil
(limit-value 4000 4000 ">" 1351)

;; (defun range-limiter (ranges rule)
;;   (let ((ranges (("x" 4000)
;;                  ("m" 4000)
;;                  ("a" 4000)
;;                  ("s" 4000))))
;;     (range-limiter (("x" 4000)
;;                     ("m" 4000)
;;                     ("a" 2005)
;;                     ("s" 4000))
;;                    (rule-for "qkq"))

;;     (ACCEPTED (("x" 4000)
;;                ("m" (- 4000 2089))
;;                ("a" (- 4000 2005))
;;                ("s" 4000)))

;;     (range-limiter (("x" 4000)
;;                     ("m" 4000)
;;                     ("a" 2005)
;;                     ("s" 4000))
;;                    (rule-for "rfg"))))



#+nil
(+ (* 2005 2089 4000 4000)
   (* 1716 4000 4000 4000)
   (* 4000 * 4000 * 4000 * 4000)
   (* 538 2440 4000 4000)
   (* (- 4000 3448) 4000 4000 4000)
   (* (+ 1415 (- 4000 2662)) 4000 4000 4000)
   (* "in")
   (* "qqz")
   ;; (* 0 "gd")
   (* (- 4000 839) (* 838 1715 4000 4000)))


#+nil
(part1 "input.txt")

#+nil
(load-file "example.txt")

#+nil
(process-rating-through-workflow
 (parse-rating "{x=2127,m=2000,a=3000,s=1013}")
 (parse-workflow "px{a<2006:qkq,m>2090:A,rfg}"))

#+nil
(apply-rule
 (car (mapcar #'parse-rule (ppcre:split "," "a<2006:qkq,m>2090:A")))
 (parse-rating "{x=2127,m=1623,a=2000,s=1013}"))

#+nil
(rules (parse-workflow "px{a<2006:qkq,m>2090:A,rfg}"))

; => ("px" (("a" "<" 2006 "qkq") ("m" ">" 2090 "A") "rfg"))

#+nil
(ppcre:split "," "a<2006:qkq,m>2090:A,rfg")
 ; => ("a<2006:qkq" "m>2090:A" "rfg")
 ; => ("px" "a<2006:qkq,m>2090:A,rfg")

#+nil
(ppcre:register-groups-bind (category operator condition result)
    ("([xmas])(<|>)([0-9]+):([a-zA-Z]+)" "a<2006:qkq")
  (list category operator condition result))

#+nil
(ppcre:register-groups-bind (name workflow-details) "([a-z]+){([^}]+)}" "px{a<2006:qkq,m>2090:A,rfg}")
