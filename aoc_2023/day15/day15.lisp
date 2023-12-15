(in-package :aoc_2023)

(ql:quickload :cl-ppcre)
(ql:quickload :arrows)

(use-package :arrows)

(defparameter *day-number* 15)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-inputs-pathname (filename)
    (arrows:-> filename
               (merge-pathnames (format nil "day~d/inputs/" *day-number*))
               (merge-pathnames #p"/home/alex/src/workspace/advent_of_code/aoc_2023/"))))

(defparameter *input* (get-inputs-pathname "input.txt"))


(defun step-current-value (current-value c)
  (-> (char-code c)
      (+ current-value)
      (* 17)
      (mod 256)))

(defun hash (string)
  (reduce #'step-current-value string :initial-value 0))

(defparameter *boxes* (make-hash-table :size 256))

(defun clear-boxes ()
  (clrhash *boxes*))

(defun lookup-in-box (str)
  (let ((box (gethash (hash str) *boxes*)))
    (assoc str box :test #'equal)))

(defun overwrite-box (box-number value)
  (setf (gethash box-number *boxes*) value))

(defun store-in-box (box-number str focal-length)
  (let ((current-box (gethash box-number *boxes*)))
    (if (assoc str current-box :test #'equal)
        (rplacd (assoc str current-box :test #'equal)
                focal-length)
        (setf (gethash box-number *boxes*)
              (cons (cons str focal-length) current-box)))))


(defun remove-in-box (str)
  (let* ((box-number (hash str))
         (box (gethash box-number *boxes*)))
    (let ((new-val (remove-from-alist str box :test #'equal)))
      (overwrite-box box-number new-val))))

(defun remove-from-alist (item alist &key (test #'eql))
  (remove (assoc item alist :test test)
          alist
          :test test))

(defun process-removal (string)
  (multiple-value-bind (start end) (ppcre:scan "-" string)
    (let ((label (subseq string 0 start)))
      (remove-in-box label))))


(defun process-addition (string)
  (destructuring-bind (label focal-length) (ppcre:all-matches-as-strings "[^=]+"
                                                                         string)
    (let* ((box-number (hash label)))
      (store-in-box box-number label (parse-integer focal-length)))))

(defun process-command (string)
  (cond
    ((ppcre:scan "-" string) (process-removal string))
    (t                       (process-addition string))))

(defun get-input-line (filename)
  (car
   (lib:read-file-lines (get-inputs-pathname filename))))

(defun split-file-line-into-commands (line)
  (ppcre:split "," line))

(defun focusing-power (focal-length box-number slot-number)
  (-> box-number
      (+ 1)
      (* slot-number)
      (* focal-length)))

(defun part2 (filename)
  (clear-boxes)

  (->> (get-input-line filename)
       (split-file-line-into-commands)
       (mapcar #'process-command))

  (loop for box-number from 0 below 256
        sum
        (let* ((box (gethash box-number *boxes*))
               (num-lenses (length box)))
          (loop for lens in box
                and idx from 0
                sum
                (let ((slot-number (- num-lenses idx)))
                  (focusing-power (cdr lens) box-number slot-number))))))

(part2 "input.txt")
