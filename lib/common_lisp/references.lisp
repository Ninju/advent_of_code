(load "~/quicklisp/setup.lisp")

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


(ppcre:register-groups-bind ((#'parse-integer game-number) choices)
    ("Game ([0-9]+): (.*;)*" "Game 1: 10 red, 7 green, 3 blue; 5 blue, 3 red, 10 green; 4 blue, 14 green, 7 red; 1 red, 11 green; 6 blue, 17 green, 15 red; 18 green, 7 red, 5 blue" :start 0)
  (format nil "This is Game ~d, with choices ~% ~{~a~^~% ~}" game-number (ppcre:split ";" choices)))

(ppcre:do-scans (mstart mend rstart rend "[0-9]" "move 19 from 1 to 10")
  (format t "mmm.. what am i supposed to do with ~d ~d ~d ~d ~%" mstart mend rstart rend))

;; one match at a time
(ppcre:do-matches-as-strings (match "[0-9]+" "move 19 from 1 to 10")
  (format t "MATCH: ~d ~%" match))

(ppcre:do-register-groups
    (what from to)
    ("move ([0-9]+) from ([0-9]+) to ([0-9]+)"
     "move 19 from 1 to 10")
  (format t "MOVE: ~d FROM: ~d TO: ~d. THANKS~%" what from to))

(ppcre:register-groups-bind
    ((#'parse-integer what from to)) ;; can even call functions on matches!
    ("move ([0-9]+) from ([0-9]+) to ([0-9]+)"
     "move 19 from 1 to 10; move 18 from 10 to 6")
  (list what from to))

;; (ppcre:do-scans (m1 m2 r1 r2
;;   "move ([0-9]+) from ([0-9]+) to ([0-9]+)"
;;   "move 19 from 4 to 8; move 14 from 10 to 3")

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

(defparameter *random* (loop repeat 1000 collect (random 10000)))
;; aggregration keywords for loop
(loop for i in *random*
   counting (evenp i) into evens
   counting (oddp i) into odds
   summing i into total
   maximizing i into max
   minimizing i into min
   finally (return (list min max total evens odds)))

;; destructuring in loop
(loop for (item . rest) on (list 1 2 3)
    do (format t "~a" item)
    when rest do (format t ", "))

;; conditional looping
(loop for i from 1 to 10
      when (evenp i)
      sum i into total
      collect (list i total))

;; named loops
(loop named outer for list in (list (list 1 2 3) (list 4 5 6) (list 7 8 9)) do
     (loop for item in list do
          (if (> item 3)
            (return-from outer item))))

;; loop // thereis / always / never return T or NIL, e.g.
(if (loop for n in (list 2 3 7 11 13 17) thereis (evenp n))
    (format nil "Yes, at least one even number is prime")
    (format nil "All primes are odd"))


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


;; PARALLEL PROCESSING
(ql:quickload "serapeum") ;; get number of CPUs
(ql:quickload "lparallel")

 (defun show-kernel-info ()
           (let ((name (lparallel:kernel-name))
                 (count (lparallel:kernel-worker-count))
                 (context (lparallel:kernel-context))
                 (bindings (lparallel:kernel-bindings)))
             (format t "Kernel name = ~a~%" name)
             (format t "Worker threads count = ~d~%" count)
             (format t "Kernel context = ~a~%" context)
             (format t "Kernel bindings = ~a~%" bindings)))

;; Seems to set :parts to the number of workers in lparallel:* functions
(setf lparallel:*kernel*
      (lparallel:make-kernel (serapeum:count-cpus) :name "custom-kernel"))

(show-kernel-info)

;; ALWAYS CLOSE THE KERNEL!
(lparallel:end-kernel :wait t)

;; NOTE: uses same function for the merging of results,
;;       use `preduce-partial` for custom merge
(lparallel:preduce #'+ #(1 2 3 4 5 6)
                   :parts 2)             ;; => (reduce #'+
                                         ;;      (list
                                         ;;        (reduce #'+ #(1 2 3))
                                         ;;        (reduce #'+ #(4 5 6)))

(reduce #'* (lparallel:preduce-partial #'+ (loop for n from 0 to 100000000 collect n)))


(reduce #'+
        (list (reduce #'+ #(1 2 3)) (reduce #'+ #(4 5 6))))
