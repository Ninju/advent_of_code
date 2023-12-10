(in-package :lib)

(ql:quickload :split-sequence)
(ql:quickload "cl-ppcre")

;; NUMBERs

(defun zero-p (n)
  (= n 0))

;; STRINGS (REGEX)
(defun words (str &key (separator "\\s+"))
  (ppcre:split separator (string-trim " " str)))

(defun split-str-by-space (line)
  (split-sequence:split-sequence #\Space line))

(defun split-by (sep lst)
  "Split list by separator element"
  (split-sequence:split-sequence sep lst
                                 :test #'equal
                                 :remove-empty-subseqs t))

(defun string->integers (str &key (separator "\\s+"))
  (mapcar #'parse-integer (words str :separator separator)))

(defun greedy-match-as-strings (str)
  (remove nil (loop for idx from 0 to (length str)
    collect (ppcre:scan-to-strings digit-regex str :start idx))))

(defun concat-strings (lst)
  (format nil "~{~a~}" lst))

(defun extract-numbers-from-line (str)
  (mapcar #'parse-integer
          (ppcre:split "\\s+"
                       (string-trim " " str))))

;; FILES
(defun read-file-lines (filename)
  (uiop:read-file-lines filename))

(defun read-file-line-groups (filename &optional (separator ""))
  (split-by "" (read-file-lines filename)))

;; ASSOCIATIVE LISTS
(defun alist-str-lookup (key alist)
  (assoc key alist :test 'equal))

;; SEQUENCES (REDUCERS)
(defun sum (numbers)
  (reduce #'+ numbers))

(defun product (numbers)
  (reduce #'* numbers))

;; SEQUENCES

(defun last-element (lst)
  (car (last lst)))

(defun first-element (lst)
  (car lst))


(defun consecutive-n (n lst)
  "Group consecutive N elements together

       (consecutive-n 3 '(1 2 3 4))
        ;; => '((1 2 3) (2 3 4))

       (consecutive-n 1 '(1 2 3 4))
        ;; => ((1) (2) (3) (4))

       (consecutive-n 5 '(1 2 3 4))
        ;; => NIL
    "
  (loop for idx from 0 to (- (length lst) n)
        collect
        (loop for i from 0 below n
              collect
              (elt lst (+ i idx)))))

(defun consecutive (lst)
  "Group consecutive elements together

       (consecutive '(1 2 3))
        ;; => '((1 2) (2 3))
    "
  (consecutive-n 2 lst))

;; OR (ql:quickload :alexandria)
;;    (alexandria:iota <length> :start <start> :step <step>)
(defun gen-range (x0 x1) (loop for n from x0 to x1 collect n))

(defun pairs-n (n lst)
  "Split list into consecutive element groups of size N.

  Note: when N groups of equal size cannot be made, the remainder is returned
        as a second value, but the elements of the remainder will not be in the
        primary result

        (pairs-n 2 '(1 2 3 4))
        ;; => ((1 2) (3 4)), NIL

        (pairs-n 3 '(1 2 3 4))
        ;; => ((1 2 3)), (4)
        ;; note: remainder is returned as second value

  "
  (if (= n 0)
      (error "Cannot call pairs-n with ZERO")
      (let ((remainder NIL)
            (remainder-sym (gensym)))
        (values
         (loop for idx from 0 below (length lst) by n
               collecting (handler-case
                              (loop for i from 0 below n
                                    collect
                                    (elt lst (+ i idx)))
                            (sb-kernel:index-too-large-error (c)
                              (setf remainder (subseq lst idx))
                              remainder-sym)) into matched-pairs
               finally (return (remove-if (lambda (e) (equal e remainder-sym))
                                          matched-pairs)))

     remainder))))

(defun pairs (lst)
  "Split list into consecutive pairs of elements.

   Different from #'consecutive as elements are not repeated in the returned
   groups
  "
  (pairs-n 2 lst))

(defun seq-intersection (seq-a seq-b)
  (remove-duplicates
   (mapcar (lambda (common)
             (list common
                   (position common seq-a)
                   (position common seq-b)))
           (mapcan (lambda (checks)
                     (remove 'nil checks))
                    (loop for i from 0 below (length seq-a) collect
                        (loop for j from 0 below (length seq-b) collect
                            (if (equal (elt seq-a i) (elt seq-b j))
                                (elt seq-a i)
                                'nil)))))
   :test #'equal))

;; SEQUENCES (DEBUGGERS)
(defun print-array-contents (a)
  (destructuring-bind (n m) (array-dimensions a)
    (loop for i from 0 below n do
      (loop for j from 0 below m do
        (format t "a[~a ~a] = ~a~%" i j (aref a i j))))))

(defmacro nested-loop (syms dimensions &body body)
  "Iterates over a multidimensional range of indices.

   SYMS must be a list of symbols, with the first symbol
   corresponding to the outermost loop.

   DIMENSIONS will be evaluated, and must be a list of
   dimension sizes, of the same length as SYMS.

   Example:
    (nested-loop (i j) '(10 20) (format t '~a ~a~%' i j))

  "
  (unless syms (return-from nested-loop `(progn ,@body))) ; No symbols

  ;; Generate gensyms for dimension sizes
  (let* ((rank (length syms))
         (syms-rev (reverse syms)) ; Reverse, since starting with innermost
         (dims-rev (loop for i from 0 below rank collecting (gensym))) ; innermost dimension first
         (result `(progn ,@body))) ; Start with innermost expression
    ;; Wrap previous result inside a loop for each dimension
    (loop for sym in syms-rev for dim in dims-rev do
         (unless (symbolp sym) (error "~S is not a symbol. First argument to nested-loop must be a list of symbols" sym))
         (setf result
               `(loop for ,sym from 0 below ,dim do
                     ,result)))
    ;; Add checking of rank and dimension types, and get dimensions into gensym list
    (let ((dims (gensym)))
      `(let ((,dims ,dimensions))
         (unless (= (length ,dims) ,rank) (error "Incorrect number of dimensions: Expected ~a but got ~a" ,rank (length ,dims)))
         (dolist (dim ,dims)
           (unless (integerp dim) (error "Dimensions must be integers: ~S" dim)))
         (destructuring-bind ,(reverse dims-rev) ,dims ; Dimensions reversed so that innermost is last
           ,result)))))

(defun max-width (list-of-lists)
  (reduce #'max list-of-lists :key #'length))

(defun get-column (2d-array col-no)
    (destructuring-bind (w h) (array-dimensions 2d-array)
      (loop for n from 0 below w
            collect
            (aref 2d-array n col-no))))

(defun lists->2d-array (strings &key (initial-element nil))
  (let* ((height (length strings))
         (number-of-cols (max-width strings))
         (result (make-array (list height number-of-cols)
                             :initial-element initial-element)))
    (loop for j from 0 below height
          do
             (let ((row (elt strings j)))
               (loop for i from 0 below (length row)
                     do
                        (let ((cur (elt row i)))
                          (setf (aref result j i) cur)))))
          result))


