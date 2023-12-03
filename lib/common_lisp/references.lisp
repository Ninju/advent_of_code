;; REGULAR EXPRESSIONS
(ql:quickload "cl-ppcre")

(ppcre:scan "def" "abcdefghi" :start 3)

(ppcre:scan-to-strings "def" "abcdefghi" :start 3)
 ; => "def", #()

(ppcre:scan-to-strings "abc" "abcdefghi" :start 3)
 ; => NIL

(ppcre:all-matches-as-strings "abc" "abcdefghi")
 ; => ("abc")

(ppcre:all-matches-as-strings "a" "abcdeafahi")
 ; => ("a" "a" "a")

(ppcre:all-matches-as-strings "abc" "abcdefghi" :start 3)
 ; => NIL

(ppcre:all-matches "a" "abcdeafahi")
 ; => (0 1 5 6 7 8)

(ppcre:all-matches "ab" "abcdeafabhi")
 ; => (0 2 7 9)

;; -- LISTS
(remove nil (list 1 2 'nil 3 4 'nil 5))
 ; => (1 2 3 4 5)

;; -- LOOP

;; loop through string using collect (same as map)
(let* ((str "abcdefghi")
       (str-length (length str))
       (maxidx (- str-length 1)))
  (loop for idx from 0 to maxidx
        collect (subseq str 0 (1+ idx))))
 ; => ("a" "ab" "abc" "abcd" "abcde" "abcdef" "abcdefg" "abcdefgh" "abcdefghi")

;; Using downfrom
(loop for idx downfrom 5 to 0
      collect idx)
 ; => (5 4 3 2 1 0)

;; loop with below keyword - useful for indicies
(loop for idx from 0 below (length (list 1 2 3 4 5))
      collect idx)
 ; => (0 1 2 3 4)


(reduce #'+ '(1 2 3 4 5 6 7 8 9 10)
        :start 2 :end 5)
 ; => 12 (4 bits, #xC, #o14, #b1100)

(reduce #'cons '(1 2 3))
 ; => ((1 . 2) . 3)
 ; (cons (cons 1 2) 3)

;; The Onion reduction method
(reduce #'cons '(1 2 3) :from-end t)
 ; => (1 2 . 3)
 ;; (cons 1 (cons 2 3))

;; :key lets you apply a function to an element before reduction
(reduce #'+ '((1) (2) (3)) :key #'car)
 ; => 6 (3 bits, #x6, #o6, #b110)


;; -- VECTORS
;; Creating vectors
(vector)
(vector 1 2 3)

;; Literal syntax
#(1 2 3)

;; MAKE-ARRAY is more general than VECTOR since you can use it to create arrays of any dimensionality as well as both fixed-size and resizable vectors.
;;      - requires a list of dimensions as argument
(make-array '(2 2) :initial-element "a")

;; use MAKE-ARRAY with `:adujstable t` for resizable vectors
;;          (vector-push 4 (vector 1 2 3)) ;=> ERROR
;;          (make-array 5 :fill-pointer 0) ;=> Not actually resizable; but you can add/remove
(make-array 5 :fill-pointer 0 :adjustable t)


(count 1 #(1 2 1 2 3 1 2 3 4))         ;; 3
(remove 1 #(1 2 1 2 3 1 2 3 4))        ;; #(2 2 3 2 3 4)
(remove 1 '(1 2 1 2 3 1 2 3 4))        ;; (2 2 3 2 3 4)
(remove #\a "foobarbaz")               ;; "foobrbz"
(substitute 10 1 #(1 2 1 2 3 1 2 3 4)) ;; #(10 2 10 2 3 10 2 3 4)
(substitute 10 1 '(1 2 1 2 3 1 2 3 4)) ;; (10 2 10 2 3 10 2 3 4)
(substitute #\x #\b "foobarbaz")       ;; "fooxarxaz"
(find 1 #(1 2 1 2 3 1 2 3 4))          ;; 1
(find 10 #(1 2 1 2 3 1 2 3 4))         ;; NIL
(position 1 #(1 2 1 2 3 1 2 3 4))      ;; 0

;; subseq - seq (start idx) (end idx + 1)
(subseq "foobarbaz" 3)   ;; "barbaz"
(subseq "foobarbaz" 3 6) ;; "bar"

(setq example "vJrwpWtwJgWrhcsFMMfFFhFp")
(subseq example 0 12)
 ; => "vJrwpWtwJgWr"
(subseq example 12 24)
 ; => "hcsFMMfFFhFp"
(subseq example 12 (length example)) ;; include the ending index (??? but ok)
 ; => "hcsFMMfFFhFp"
(subseq example 12 30) ;; -- ERROR out of bounds

(setq astring "vJrwpWtwJgWr")
(setq bstring "hcsFMMfFFhFp")

;; loop 2 dimension array
(loop for i from 0 below (length astring) collect
      (loop for j from 0 below (length bstring) collect
            (if (equal (elt astring i) (elt bstring j))
                (elt astring i)
                'nil)))


(defparameter *x* (copy-seq "foobarbaz"))

(setf (subseq *x* 3 6) "xxx")  ; subsequence and new value are same length
*x* ;; "fooxxxbaz"

(setf (subseq *x* 3 6) "abcd") ; new value too long, extra character ignored.
*x* ;; "fooabcbaz"

(setf (subseq *x* 3 6) "xx")   ; new value too short, only two characters changed
*x* ;; "fooxxcbaz"

(position #\b "foobarbaz") ;; 3
(search "bar" "foobarbaz") ;; 3

;; BITS & BYTES

;; literal binary number syntax
#*101011
