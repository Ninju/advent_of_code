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
   :read-file-line-groups

   ;; 2D ARRAYS
   :print-array-contents))
