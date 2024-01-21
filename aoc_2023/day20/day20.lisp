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

;; HELPERS

(defun array->list (arr)
  (loop for a across arr collect a))

(defparameter +low-pulse+ 1)
(defparameter +high-pulse+ 2)

(defun pulse (sym)
  (cond ((eql sym :high) +high-pulse+)
        ((eql sym :low) +low-pulse+)
        (t (error "Unrecognised pulse, expected :low or :high"))))

(defun low-pulse-p (pulse) (= +low-pulse+ pulse))
(defun high-pulse-p (pulse) (= +high-pulse+ pulse))

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

;; DATA STRUCTURES

(defstruct (queue (:constructor %make-queue)) contents)

(defun make-queue (&rest initial-contents)
  (%make-queue :contents initial-contents))

(defun queue-push (queue &rest elements)
  (with-slots (contents) queue
    (setf contents (append contents elements))))

(defun queue-pop (queue)
  (with-slots (contents) queue
    (prog1 (car contents)
      (setf contents (cdr contents)))))

(defun queue->list (queue)
  (with-slots (contents) queue
    contents))

(defclass module (standard-object)
  ((id :initarg :id :reader id)
   (high-pulses-received-count :initform 0 :accessor high-pulses-received-count)
   (low-pulses-received-count :initform 0 :accessor low-pulses-received-count)
   (outputs :initarg :outputs
            :reader outputs)
   (received-pulses :accessor received-pulses
                    :initform (make-queue))))

(defclass flip-flop (module)
  ((state :initform :off :initarg :state :accessor state)))

(defclass conjunction (module)
  ((inputs :initform NIL
           :initarg :inputs
           :accessor inputs)
   (received-from-inputs-map :initform NIL
                             :accessor received-from-inputs-map)))

(defclass broadcaster (module) ())
(defclass output-module (module) ())

(defgeneric receive-pulse (module sender pulse))
(defgeneric dispatch (module))
(defgeneric conjunction-p (module))

(defmethod initialize-instance :after ((module output-module) &key)
  (setf (slot-value module 'outputs) '()))

(defmethod conjunction-p ((module conjunction)) T)
(defmethod conjunction-p ((module T)) NIL)

(defmethod print-object ((object module) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (id outputs) object
      (format stream "~s -> (~{~s~^ ~})" id outputs))))

(defmethod print-object ((object output-module) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~s" (id object))))

(defun dispatch-pulse-to-outputs (dispatcher pulse)
  (mapcar (lambda (output) (list (id dispatcher) pulse output))
          (outputs dispatcher)))

(defmethod receive-pulse ((module module) sender pulse)
  (cond ((high-pulse-p pulse) (incf (high-pulses-received-count module)))
        ((low-pulse-p pulse) (incf (low-pulses-received-count module))))

  (queue-push (received-pulses module) pulse))

(defun alist-update (key alist new-value &rest args)
  (let ((args-with-key (append args (list :key #'car))))
    (funcall #'acons key new-value
             (apply #'remove key alist args-with-key))))

(defmethod receive-pulse ((module conjunction) sender pulse)
  (with-accessors ((received-from-inputs-map received-from-inputs-map)) module
    (setf received-from-inputs-map
          (alist-update sender received-from-inputs-map pulse :test #'equal)))

  (call-next-method))

(defmethod receive-pulse ((module T) sender pulse)
  (call-next-method))

(defmethod dispatch ((module module))
  (with-slots (received-pulses) module
    (dispatch-pulse-to-outputs module (queue-pop received-pulses))))

(defmethod dispatch ((module flip-flop))
  (with-slots (received-pulses) module
    (let ((pulse (queue-pop received-pulses)))
      (if (low-pulse-p pulse)
          (progn
            (setf (state module) (flip-state (state module)))
            (if (eql :on (state module))
                (dispatch-pulse-to-outputs module (pulse :high))
                (dispatch-pulse-to-outputs module (pulse :low))))))))

(defmethod dispatch ((module conjunction))
  (with-slots (received-from-inputs-map inputs) module
    (let ((pulse-to-dispatch
            (if (not
                 (every (lambda (input)
                          (eql (pulse :high)
                               (alexandria:assoc-value received-from-inputs-map
                                                       input
                                                       :test #'equal)))
                        inputs))
                (pulse :high)
                (pulse :low))))

      (dispatch-pulse-to-outputs module pulse-to-dispatch))))

;; FILE PARSING

(defun build-module-network (modules)
  (let ((hsh (make-hash-table :size (* 2 (length modules)) :test #'equal)))
    (loop for m in modules do
      (setf (gethash (id m) hsh) m))
    hsh))

(defun parse-module (line)
  (ppcre:register-groups-bind (modifier id outputs)
      ("([%&])?([a-zA-Z]+)\\s*->\\s*(.*)" (string-trim '(#\Space) line))
    (let ((output-ids (ppcre:split "\\s*,\\s*" outputs)))
      (make-instance
       (cond
         ((string= "%" modifier) 'flip-flop)
         ((string= "&" modifier) 'conjunction)
         (t                      'module))
       :id id
       :outputs output-ids))))

(defun modules-mapping-to-conjunctions (modules)
  (let ((conjunction-ids (->> (remove-if-not #'conjunction-p modules)
                              (mapcar #'id)))
        (other-modules (remove-if #'conjunction-p modules)))

    (loop for m in other-modules
          append
          (loop for c in (intersection conjunction-ids (outputs m) :test #'equal)
                collect (cons m c)))))

(defun scan-all-outputs-for-conjunctions (modules)
  (group-by:group-by (modules-mapping-to-conjunctions modules)
                   :key #'cdr
                   :value #'car
                   :test #'equal))

(defun ensure-conjunctions-know-their-inputs (modules)
  (loop for res in (scan-all-outputs-for-conjunctions modules)
        do
           (destructuring-bind (id &rest inputs) res
             (let ((conj (find id modules :key #'id :test #'equal)))
               (setf (inputs conj) (mapcar #'id inputs)))))

        modules)

(defun distinct-outputs (modules)
  (remove-duplicates (apply #'append (mapcar #'outputs modules)) :test #'equal))

(defun find-orphan-module-outputs (modules)
  (remove-if (lambda (o)
               (member o modules :test #'equal :key #'id))
             (distinct-outputs modules)))

(defun ensure-all-outputs-point-to-module (modules)
  (append modules
          (mapcar (lambda (id) (make-instance 'output-module :id id))
                  (find-orphan-module-outputs modules))))

(defun load-modules-from-file (filename)
  (->> (get-inputs-pathname filename)
       (lib:read-file-lines)
       (mapcar #'parse-module)
       (ensure-conjunctions-know-their-inputs)
       (ensure-all-outputs-point-to-module)))

;; BUSINESS LOGIC

(defun find-module (id module-network)
  (gethash id module-network))

(defun distinct-routes (routing)
  (remove-duplicates routing :test #'equal
                     :key (lambda (k)
                            (destructuring-bind (source-id pulse destination-id) k
                              (declare (ignore source-id) (ignore pulse))
                              destination-id))))

(defun step-once (module-network routing)
  (loop for (source-id pulse destination-id) in routing
        append
        (progn
          (let ((destination-mod (find-module destination-id module-network)))
            (format t "~&~a -~a-> ~a"
                    source-id
                    (pulse->string pulse)
                    destination-id)

            (receive-pulse destination-mod source-id pulse)
            (dispatch destination-mod)))))

(defparameter *iter-limit* 1000)
(defun step-until-done (module-network initial-routing)
  (loop
    for routing = initial-routing then (step-once module-network routing)
    do
       (if (not routing)
           (return-from step-until-done "DONE"))

       (protect-against-infinite-loop!)
    ))

(defun initial-routing (initial-pulse)
  (list (list "button" initial-pulse "broadcaster")))

(defun press-button (module-network &optional (initial-pulse (pulse :low)))
  (let ((routing (initial-routing initial-pulse)))
    (step-until-done module-network routing)))

(defun press-button-n-times (n-times module-network)
  (dotimes (k n-times)
    (progn
      (format t "~2&Pressed ~D times" (+ 1 k))
      (step-until-done module-network
                       (initial-routing (pulse :low)))))

  (loop for key in (alexandria:hash-table-keys module-network)
        for mod = (find-module key module-network)
        summing (low-pulses-received-count mod) into low-count
        summing (high-pulses-received-count mod) into high-count
        finally (return (list low-count high-count))))

(defun part1 (filename)
  (let* ((modules (load-modules-from-file filename))
         (module-network (build-module-network modules)))
    (->> (press-button-n-times 1000 module-network)
         (reduce #'*))))

#+nil
(format t "~25&")

#+nil
(with-open-file (*standard-output* "standard_output.log"
                                   :direction :output
                                   :if-exists :supersede)
  (part1 "input.txt"))

 ; => 1092834855 (31 bits, #x41235627)
; Pressed 1000 times => (26145 41799)

#+nil
(with-open-file (*standard-output* "standard_output.log"
                                   :direction :output
                                   :if-exists :supersede)
  (part1 "interesting_example.txt"))

#+nil
(let* ((modules (load-modules-from-file "input.txt"))
       (module-network (build-module-network modules)))
  (press-button-n-times 1000 module-network))

#+nil
(->> (load-modules-from-file "input.txt"))

#+nil
(let ((modules (load-modules-from-file "loop_example.txt")))
  (alexandrdia:hash-table-alist (build-module-network modules)))

#+nil
(let ((broadcaster (make-instance 'broadcaster :id "broadcaster" :outputs (list "a" "b" "c"))))
  (receive-pulse broadcaster "inv" (pulse :low))
  (receive-pulse broadcaster "inv" (pulse :low))
  (receive-pulse broadcaster "inv" (pulse :low))
  (receive-pulse broadcaster "inv" (pulse :low))
  (low-pulses-received-count broadcaster)
  (dispatch broadcaster)
  (dispatch broadcaster)
  (received-pulses broadcaster))

