(load "~/quicklisp/setup.lisp")

(in-package :lib)

(ql:quickload :split-sequence)
(ql:quickload "cl-ppcre")

(defun alist-str-lookup (key alist)
  (assoc key alist :test 'equal))

(defun sum (numbers)
  (reduce #'+ numbers))

(defun split-str-by-space (line)
  (split-sequence:split-sequence #\Space line))

(defun split-by (sep lst)
  "Split list by separator element"
  (split-sequence:split-sequence sep lst
                                 :test #'equal
                                 :remove-empty-subseqs t))

(defun read-file-lines (filename)
  (uiop:read-file-lines filename))

(defun read-file-line-groups (filename &optional (separator ""))
  (split-by "" (read-file-lines filename)))

(defun greedy-match-as-strings (str)
  (remove nil (loop for idx from 0 to (length str)
    collect (ppcre:scan-to-strings digit-regex str :start idx))))

(defun concat-strings (lst)
  (format nil "~{~a~}" lst))

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

  (nested-loop (i j) '(3 3)
    (format nil '~d ~d~%' i j))

(format nil "(~{~a~^ ~})" (list 1 2 3))

(loop for i from 10 repeat 20 do
      (if (= i 49)
          (return :else)))
