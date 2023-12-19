(in-package :aoc_2023)

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

;; DATA STRUCTURES

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
