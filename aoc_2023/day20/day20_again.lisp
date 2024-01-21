(in-package :aoc_2023)

(ql:quickload :arrows)
(ql:quickload :alexandria)
(ql:quickload :cl-ppcre)
(ql:quickload :make-hash)
(ql:quickload :group-by)

(use-package :arrows)

(declaim (optimize (debug 3)))

;; PROGRAM HELPERS

(defparameter *day-number* 20)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-inputs-pathname (filename)
    (-> filename
        (merge-pathnames (format nil "day~d/inputs/" *day-number*))
        (merge-pathnames #p"/home/alex/src/workspace/advent_of_code/aoc_2023/"))))

(defparameter *iter-limit* 100000)

(defmacro protect-against-infinite-loop! ()
  `(progn
     (decf *iter-limit*)
     (if (< *iter-limit* 0)
         (progn (setf *iter-limit* 0)
                (error "Reached iteration limit!")))))

(defparameter +low-pulse+ NIL)
(defparameter +high-pulse+ T)

(defun pulse (sym)
  (cond ((eql sym :high) +high-pulse+)
        ((eql sym :low) +low-pulse+)
        (t (error "Unrecognised pulse, expected :low or :high"))))

(defun low-pulse-p (pulse) (eq +low-pulse+ pulse))
(defun high-pulse-p (pulse) (eq +high-pulse+ pulse))

(defun opposite-pulse (pulse)
  (cond ((low-pulse-p pulse) (pulse :high))
        ((high-pulse-p pulse) (pulse :low))))

(defun pulse->string (pulse)
  (cond ((high-pulse-p pulse) "high")
        ((low-pulse-p pulse) "low")))

(defun flip-state (state)
  (cond ((eql :off state) :on)
        ((eql :on state) :off)
        (t (error "Unrecognised state, expected :on or :off"))))

(defun alist-update (key alist new-value &rest args)
  (let ((args-with-key (append args (list :key #'car))))
    (funcall #'acons key new-value
             (apply #'remove key alist args-with-key))))

(defstruct (module (:constructor %make-module)) id type outputs)
(defstruct (flip-flop (:constructor %make-flip-flop)) id state type outputs)
(defstruct (conjunction (:constructor %make-conjunction)) id type outputs inputs-received)
(defstruct (broadcaster (:constructor %make-broadcaster)) outputs)
(defstruct (output (:constructor %make-output)) outputs)

(defun make-module (id type outputs)
  (%make-module :id id
                :type type
                :outputs outputs))

(defun make-flip-flop (id state type outputs)
  (%make-flip-flop :id id
                   :state state
                   :type type
                   :outputs outputs))

(defun make-broadcaster (outputs)
  (%make-broadcaster :outputs outputs))

(defun make-output ()
  (%make-output :outputs '()))

(defun make-conjunction (id type outputs inputs)
  (%make-conjunction :id id :type type :outputs outputs :inputs-received '()))

(defun mod-type (mod)
  (with-slots (type) mod
    type))

(defun outputs (mod)
  (with-slots (outputs) mod
    outputs))

(defun flip-state (flip-flop)
  (with-accessors ((state state)) flip-flop
    (cond
      ((eq :on  state) (setf state :off))
      ((eq :off state) (setf state :on)))))

(defun regular-dispatch (destinations pulse)
  (mapcar (lambda (d) (cons d pulse)) destination))

(defun conj-remember (conj sender pulse)
  (with-slots ((sender-id id)) sender
    (with-accessors ((inputs-received inputs-received)) conj
      (setf inputs-received
            (alist-update sender-id inputs-received pulse :test #'equal)))))

(defun conj-inputs-all-high-p (conj)
  (with-slots (all-inputs inputs-received) conj
    (not-implemented "need to store all inputs, and then loop through to check against inputs received")
    (every (lambda (input)
             (eql :high
                  (alexandria:assoc-value inputs-received input :test #'equal)))
           all-inputs)))

(defun send-pulse-to (sender pulse destination)
  (cond
    ((string= "&" (mod-type destination)) (progn
                                            (conj-remember destination sender pulse)
                                            (if (conj-inputs-all-high-p destination)
                                                (regular-dispatch (outputs destination) :low))))
    ((string= "%" (mod-type destination)) (progn
                                            (when (eq :low pulse)
                                              (flip-state destination)
                                              (if (eq :on (flip-flop-state destination))
                                                  (regular-dispatch (outputs destination) :high)))))

    (t (regular-dispatch (outputs destination) pulse))))

(let ((modules (make-hash-table :size 1000 :test #'equal)))
  (loop for line in (lib:read-file-lines (get-inputs-pathname "loop_example.txt"))
        collect
           (ppcre:register-groups-bind (modifier id outputs)
               ("([%&])?([a-zA-Z]+)\\s*->\\s*(.*)" (string-trim '(#\Space) line))
             (let ((output-ids (ppcre:split "\\s*,\\s*" outputs)))
               (cond
                 ((string= "%" modifier) (make-flip-flop id :off modifier output-ids))
                 ((string= "&" modifier) (send-pulse-to NIL T (make-conjunction id modifier output-ids NIL)))
                 ((string= id "broadcaster") (make-broadcaster outputs))
                 (t (make-output)))))))
