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

(defparameter +low-pulse+ 0)
(defparameter +high-pulse+ 1)

(defun pulse (sym)
  (cond ((eql sym :high) +high-pulse+)
        ((eql sym :low) +low-pulse+)))

(defun low-pulse-p (pulse) (= +low-pulse+ pulse))
(defun high-pulse-p (pulse) (= +high-pulse+ pulse))

(defun opposite-pulse (pulse)
  (cond ((low-pulse-p pulse) +high-pulse+)
        ((high-pulse-p pulse) +low-pulse+)))

(defun pulse->string (pulse)
  (cond ((high-pulse-p pulse) "high")
        ((low-pulse-p pulse) "low")))

(defun flip-state (state)
  (cond ((eql :off state) :on)
        ((eql :on state) :off)))

;; DATA STRUCTURES

(defclass module (standard-object)
  ((id :initarg :id :reader id)
   (high-pulses-received-count :initform 0 :accessor high-pulses-received-count)
   (low-pulses-received-count :initform 0 :accessor low-pulses-received-count)
   (outputs :initarg :outputs
            :reader outputs)
   (received-pulses :accessor received-pulses
                    :initform (make-array 100 :fill-pointer 0
                                              :adjustable t))
   (received-senders :accessor received-senders
                     :initform (make-array 100 :fill-pointer 0
                                               :adjustable t))))

(defclass flip-flop (module)
  ((state :initform :off :initarg :state :accessor state)))

(defclass conjunction (module)
  ((inputs :initform NIL
           :initarg :inputs
           :accessor inputs)
   (received-from-inputs-map :initform NIL
                             :accessor received-from-inputs-map)))

(defclass output-module (module) ())

(defmethod initialize-instance :after ((module output-module) &key)
  (setf (slot-value module 'outputs) '()))

(defmethod print-object ((object module) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (id outputs) object
      (format stream "~s -> (~{~s~^ ~})" id outputs))))

(defmethod print-object ((object output-module) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~s" (id object))))

(defgeneric receive-pulse (module sender pulse))
(defgeneric dispatch (module))

(defun dispatch-pulse-to-outputs (dispatcher pulse)
  (mapcar (lambda (output)
            (list (id dispatcher) pulse output))
          (outputs dispatcher)))

(defmethod receive-pulse ((module module) sender pulse)
  (cond ((high-pulse-p pulse) (incf (high-pulses-received-count module)))
        ((low-pulse-p pulse) (incf (low-pulses-received-count module))))

  (vector-push pulse (received-pulses module))
  (vector-push sender (received-senders module)))

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

(defun pop-sender-with-pulse (module)
  (list (vector-pop (received-senders module))
        (vector-pop (received-pulses module))))

(defmethod dispatch ((module module))
  (destructuring-bind (_sender pulse) (pop-sender-with-pulse module)
    (declare (ignore _sender))

    (dispatch-pulse-to-outputs module pulse)))

(defmethod dispatch ((module flip-flop))
  (destructuring-bind (_sender pulse) (pop-sender-with-pulse module)
    (declare (ignore _sender))

    (if (low-pulse-p pulse)
        (progn
            (setf (state module) (flip-state (state module)))
            (if (eql :on (state module))
                (dispatch-pulse-to-outputs module (pulse :high))
                (dispatch-pulse-to-outputs module (pulse :low)))))))

(defun array->list (arr)
  (loop for a across arr collect a))

(defmethod dispatch ((module conjunction))
  (with-slots (received-from-inputs-map inputs) module
    (let ((pulse-to-dispatch
            (if (every (lambda (input)
                         (eql (pulse :high)
                              (alexandria:assoc-value received-from-inputs-map
                                                      input
                                                      :test #'equal)))
                       inputs)
                (pulse :low)
                (pulse :high))))

      (dispatch-pulse-to-outputs module pulse-to-dispatch))))

(defgeneric conjunction-p (module))
(defmethod conjunction-p ((module conjunction)) T)
(defmethod conjunction-p ((module T)) NIL)

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

(defun routing-to-output-p (routing)
  (destructuring-bind (source-id pulse destination-id) routing
      (declare (ignore source-id) (ignore pulse))
    (string= destination-id "output")))

(defun step-once (module-network routing)
  (loop for (source-id pulse destination-id) in routing
        append
        (progn
          (let ((destination-mod (find-module destination-id module-network)))
            ;; (format t "~&~a -~a-> ~a"
            ;;         source-id
            ;;         (pulse->string pulse)
            ;;         destination-id)

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
        for mod = (gethash key module-network)
        summing (low-pulses-received-count mod) into low-count
        summing (high-pulses-received-count mod) into high-count
        finally (return (list low-count high-count))))

(defun part1 (filename)
  (let* ((modules (load-modules-from-file filename))
         (module-network (build-module-network modules)))
    (->> (press-button-n-times 1000 module-network))))

#+nil
(format t "~25&")

#+nil
(part1 "input.txt")
; Pressed 1000 times => (26145 41799)


#+nil
(let* ((modules (load-modules-from-file "interesting_example.txt"))
       (module-network (build-module-network modules)))
  (press-button-n-times 1000 module-network))

#+nil
(->> (load-modules-from-file "input.txt"))

#+nil
(let* ((modules (load-modules-from-file "loop_example.txt"))
       (module-network (build-module-network modules)))
  (destructuring-bind (source-id pulse destination-id)
      '("broadcaster" 0 "a")
    (let ((mod (find-module destination-id module-network)))
      (receive-pulse mod source-id pulse)
      (received-senders mod))))

#+nil
(let ((modules (load-modules-from-file "loop_example.txt")))
  (alexandria:hash-table-alist (build-module-network modules)))

#+nil
(let ((broadcaster (make-instance 'module :id "broadcaster" :outputs (list "a" "b" "c"))))
  (receive-pulse broadcaster "inv" (pulse :low))
  (receive-pulse broadcaster "inv" (pulse :low))
  (receive-pulse broadcaster "inv" (pulse :low))
  (receive-pulse broadcaster "inv" (pulse :low))
  (low-pulses-received-count broadcaster)
  (dispatch broadcaster)
  (dispatch broadcaster)
  (received-pulses broadcaster))
