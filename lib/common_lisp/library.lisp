(load "~/quicklisp/setup.lisp")

(defpackage :lib
  (:use :cl)
  (:export
   ;; STRINGS
   :concat-strings
   :split-str-by-space
   :greedy-match-as-strings

   ;; ASSOC lists
   :alist-str-lookup

   ;; LIST HELPERS
   :split-by
   :sum
   :seq-intersection

   ;; FILE READING
   :read-file-lines
   :read-file-line-groups))

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

(position #\e (subseq "abcdefgh" 0))
(position #\e (subseq "abcdefgh" 3))
