(in-package :aoc_2022)

(ql:quickload :split-sequence)
(ql:quickload :cl-ppcre)

(defparameter example-input "/home/alex/src/workspace/advent_of_code/aoc_lang_2022/day3/example.txt")
(defparameter input "/home/alex/src/workspace/advent_of_code/aoc_lang_2022/day3/input.txt")
(defparameter *number-scanner* (ppcre:create-scanner "[0-9]+"))

(defun get-main-lines ()
  (lib:read-file-lines input))

(defun get-example-lines ()
  (lib:read-file-lines example-input))

(defun unique-symbols-in-lines ()
  (remove-if (lambda (char)
               (position char "1234567890"))
             (remove-duplicates (reduce
                                 (lambda (a b)
                                   (concatenate 'string a b))
                                 (concatenate 'list *lines*))
                                :test #'equal)))

(defparameter *lines* (lib:read-file-lines example-input))
(defparameter *adjacent-positions*
  '((-1 -1)
    (1 1)

    (-1 0)
    (0 -1)

    (0 1)
    (1 0)

    (-1 1)
    (1 -1)))

(defun find-number-positions (str)
  (ppcre:all-matches *number-scanner* str))

(defparameter symbols "/#$-=&@%+*!(),`\\")
;; "%!#$&()\\-`+,\"*")

(defparameter *symbol-locations* (make-hash-table :test #'equal))

(defun reset-symbol-map ()
  (clrhash *symbol-locations*))

(defun build-symbol-map ()
  (loop for j from 0 below (length *lines*) do
        (let ((current-line (elt *lines* j)))
          (loop for i from 0 below (length current-line) do
            (let ((current-char (elt current-line i)))
              (if (symbol-p current-char)
                  (setf (gethash (list i j) *symbol-locations*) t)))))))

(defun symbol-at-p (i j)
  (gethash (list i j) *symbol-locations*))

(defun symbol-p (char)
  (position char symbols :test #'equal))

(defun adj-to-symbol (i j)
  (some (lambda (pos)
          (destructuring-bind (dx dy) pos
            (symbol-at-p (+ i dx)
                         (+ j dy))))
        *adjacent-positions*))

(defun find-part-numbers (line)
  (let ((numbers (find-number-positions line)))
    (if numbers
        (loop for n from 0 below (length numbers) by 2 collect
              (let ((p1 (elt numbers n))
                    (p2 (elt numbers (1+ n))))
                (cons (subseq line p1 p2)
                      (cons p1 (1- p2)))))
        '())))


(defun x-region-adj-to-symbol-p (xstart xend y)
  (reduce (lambda (a b) (or a b))
          (loop for current-idx from xstart to xend
                collect
                (if (adj-to-symbol current-idx y)
                    t
                    nil))))

(defun find-parts-next-to-symbols ()
  (let ((lines-with-parts (mapcar #'find-part-numbers *lines*)))
    (loop for j from 0 below (length lines-with-parts)
          collect
          (mapcar (lambda (current-part)
                    (destructuring-bind (part-val idx . endidx) current-part
                      (if (x-region-adj-to-symbol-p idx endidx j)
                          part-val)))
                  (elt lines-with-parts j)))))

(defun main ()
  (let ((*lines* (lib:read-file-lines input)))
    (reset-symbol-map)
    (build-symbol-map)
    (lib:sum
     (mapcar #'parse-integer
             (mapcan (lambda (parts)
                       (remove 'nil parts))
                     (find-parts-next-to-symbols))))))

(defun adjacent-part-x-p (part i)
  (destructuring-bind (part-val x1 . x2) part
    (and (>= i (1- x1))
         (<= i (1+ x2)))))

;; ( row . ( p1 . p2 ) )
(defun adjacent-parts (map i j)
  ;;(remove-if (lambda (res) (= 1 (length res)))
    (remove 'nil (loop for dj from -1 to 1
          collect
          (let* ((row-no (+ j dj))
                 (line (elt map row-no))
                 (part-numbers (find-part-numbers line))
                 (parts-on-line (remove-if-not (lambda (part)
                                                 (adjacent-part-x-p part i))
                                               part-numbers)))
            (if (not parts-on-line)
                nil
                (cons row-no parts-on-line))))))

            ;; (cons row-no (remove 'nil (remove-if-not (lambda (part) (adjacent-part-x-p part i)) part-numbers))))))
 ; => ((("264" 61 . 63) ("636" 65 . 67)) NIL (("979" 65 . 67)))

(let ((*lines* (get-main-lines)))
  (adjacent-parts *lines* 64 4))

  ;; (remove-if-not #'gear-p (find-potential-gears)))

(defun find-char (map char)
  (mapcan (lambda (res) (remove 'nil res))
          (loop for j from 0 below (length map)
                collect
                (let ((current-line (elt *lines* j)))
                  (loop for i from 0 below (length current-line)
                        collect
                        (if (equal char (elt current-line i))
                            (cons i j)))))))




(defun gear-ratio (gear-neighbours)
  (reduce #'*
          (mapcar #'parse-integer
                  (mapcar #'car
                          (mapcan #'cdr gear-neighbours)))))

;; Example gear-neighbours:
;;  '((4 ("264" 61 . 63) ("636" 65 . 67)))
;;  '((3 ("77" 50 . 51)) (5 ("390" 48 . 50)))

;; (gear-p '((4 ("264" 61 . 63) ("636" 65 . 67))))
;; (gear-p '((3 ("77" 50 . 51)) (5 ("390" 48 . 50))))

(defun gear-p (gear-neighbours)
  (= 2
     (reduce #'+
             (loop for row in gear-neighbours
                   collect
                   (length (cdr row))))))

(defun find-potential-gears ()
  (mapcar (lambda (pos)
            (destructuring-bind (i . j) pos
              (adjacent-parts *lines* i j)))
          (find-char *lines* #\*)))

(let ((*lines* (get-main-lines)))
  (reduce #'+
          (remove-if-not #'gear-p (find-potential-gears))
          :key #'gear-ratio))

(let ((*lines* (get-example-lines)))
  (remove-if-not #'gear-p
   (find-potential-gears)))

 ; => 75630868 (27 bits, #x4820914)
(main)
