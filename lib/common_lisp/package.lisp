(defpackage :lib
  (:use :cl)
  (:export
   ;; STRINGS
   :concat-strings
   :split-str-by-space
   :greedy-match-as-strings
   :extract-numbers-from-line

   ;; ASSOC lists
   :alist-str-lookup

   ;; LIST HELPERS
   :split-by
   :sum
   :seq-intersection
   :pairs-n
   :gen-range
   :consecutive
   :consecutive-n

   ;; FILE READING
   :read-file-lines
   :read-file-line-groups

   ;; 2D ARRAYS
   :print-array-contents
   :lists->2d-array
   :get-column
   :max-width
   :grid-adjacent-points-to))
