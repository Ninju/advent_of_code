(in-package :aoc_2023)

(ql:quickload :cl-ppcre)
(ql:quickload :arrows)

(use-package :arrows)

(declaim (optimize (debug 3)))

(defparameter *day-number* 12)
(defparameter *count-separator-regex* (ppcre:create-scanner "\\s*,\\s*"))
(defparameter *whitespace-regex* (ppcre:create-scanner "\\s+"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-inputs-pathname (filename)
    (arrows:-> filename
               (merge-pathnames (format nil "day~d/inputs/" *day-number*))
               (merge-pathnames #p"/home/alex/src/workspace/advent_of_code/aoc_2023/"))))

(defun create-unvisited (next-char str cursor counts cur)
  (list
   (format nil "~A~A" cur next-char)
   (+ 1 cursor)
   (consume-counts-with-char counts next-char)))

(defun consume-counts (counts amount)
  (if (> amount (car counts))
      (error "Cannot consume counts by that much!")
      (cons (- (car counts) amount)
            (cdr counts))))

(defun consume-counts-with-char (counts char)
  (if (char= #\# char)
      (consume-counts counts 1)
      counts))

(defun counts-remaining-p (counts)
  (> (count-if (lambda (n) (> n 0)) counts) 0))

(defun branch-search-arrangements (str cursor counts unvisited cur acc)
  ;; (format t "~&BRANCHING: ~a (~d)" cur cursor)
  (let ((new-unvisited (create-unvisited #\. str cursor counts cur)))

      (continue-search-arrangements #\#
                                    str
                                    cursor
                                    (consume-counts counts 1)
                                    (cons new-unvisited unvisited)
                                    cur
                                    acc)))


(defun continue-search-arrangements (new-char str cursor new-counts unvisited cur acc)
  (search-arrangements str
                       (+ 1 cursor) ;; already know the next char and have adjusted the counts accordingly
                       new-counts
                       unvisited
                       (format nil "~A~A" cur new-char)
                       acc))

(defun search-from-next-unvisited (str unvisited acc)
  (if (not unvisited)
      (progn ;; (format t "~&DONE")
             acc)
      (destructuring-bind (next-str next-cursor next-counts) (car unvisited)

        (progn
          ;; (format t "~&UNVISITED: ~a (~d)" next-str next-cursor)

          (search-arrangements str
                               next-cursor
                               next-counts
                               (cdr unvisited)
                               next-str
                               acc)))))

(defun counts-just-fully-consumed-p (counts)
  (= 0 (car counts)))

(defun counts-fully-consumed-p (counts)
  (not counts))

(defun counts-empty-p (counts)
  (not counts))

(defun safe-previous-char (str cursor)
  (if (> cursor 0)
      (elt str (- cursor 1))
      #\+))

(defun damaged-char-p (str cursor)
  (char= #\# (elt str cursor)))

(defun unknown-char-p (str cursor)
  (char= #\? (elt str cursor)))

(defun empty-char-p (str cursor)
  (char= #\. (elt str cursor)))

(defun count-possible-max-count (spring)
  (let ((matches (ppcre:all-matches-as-strings "[\\?#]" spring)))
    (if matches
        (length matches)
        0)))

(defun search-arrangements (str cursor counts unvisited cur acc)
  (cond
    ;; CHECK: at end of string?
    ((>= cursor (length str))
     (if (counts-remaining-p counts)
         (search-from-next-unvisited str unvisited acc)
         (search-from-next-unvisited str unvisited (+ 1 acc))))

    ;; CHECK: already consumed counts but have more '#'
    ((and (counts-empty-p counts)
          (ppcre:scan "#" (subseq str cursor)))
     (search-from-next-unvisited str unvisited acc))

    ((counts-empty-p counts)
     (search-from-next-unvisited str
                                 unvisited
                                 (+ 1 acc)))
    ((> (lib:sum counts)
        (count-possible-max-count (subseq str cursor)))
     (search-from-next-unvisited str
                                 unvisited
                                 acc))

    ;; CHECK: just finished consuming, next char must be a dot '.'
    ((and (counts-just-fully-consumed-p counts)
          (damaged-char-p str cursor))
     (search-from-next-unvisited str unvisited acc))

    ((and (counts-just-fully-consumed-p counts)
          (or
           (empty-char-p str cursor)
           (unknown-char-p str cursor)))
     (continue-search-arrangements #\. str cursor (cdr counts) unvisited cur acc))

    ;; CHECK: previous char was '#' and we have counts to consume, then current
    ;;        char must be '#'
    ((and (counts-remaining-p counts)
          (char= #\# (safe-previous-char str cursor))
          (or (damaged-char-p str cursor)
              (unknown-char-p str cursor)))

     (continue-search-arrangements #\#
                                   str
                                   cursor
                                   (consume-counts counts 1)
                                   unvisited
                                   cur
                                   acc))

    ((and (counts-remaining-p counts)
          (char= #\# (safe-previous-char str cursor))
          (empty-char-p str cursor))
     (search-from-next-unvisited str unvisited acc))

    ((or (damaged-char-p str cursor)
         (empty-char-p str cursor))

     (continue-search-arrangements (elt str cursor)
                                   str
                                   cursor
                                   (consume-counts-with-char counts
                                                             (elt str cursor))
                                   unvisited
                                   cur
                                   acc))

    ((unknown-char-p str cursor)
     (branch-search-arrangements str cursor counts unvisited cur acc))

    (t (error "Non-exhaustive search for string"))))


(defun search-all-arrangements (str counts)
  (search-arrangements str 0 counts '() "" 0))

(defun repeat (times lst)
  (let ((result '()))
    (dotimes (n times)
      (setf result (append lst result)))
    result))

(defun copies (times e)
  (let ((result '()))
    (dotimes (n times)
      (setf result (cons e result)))
    result))

(defun explode-input (input)
  (destructuring-bind (spring counts) input
    (list (format nil "~{~A~^?.~}" (copies 5 spring))
          (repeat 5 counts))))

(defun parse-counts-string (str)
  (mapcar #'parse-integer (ppcre:split *count-separator-regex* str)))

(defun parse-input-line (line)
  (destructuring-bind (spring counts-str)
      (ppcre:split *whitespace-regex* line)
    (list spring (parse-counts-string counts-str))))

(defun load-exploded-input (filename)
  (->> (lib:read-file-lines (get-inputs-pathname filename))
       (mapcar #'parse-input-line)
       (mapcar #'explode-input)))

#+nil
(let ((SLYNK-STICKERS:*BREAK-ON-STICKERS* (list :after)))
  (search-all-arrangements "???.###?" '(1 1 3)))

#+nil
(reduce #'+
        (load-exploded-input "example.txt")
        :key (lambda (input)
               (destructuring-bind (str counts) input
                 (search-all-arrangements str counts))))

;; Exploded test input

#+nil
(search-all-arrangements "???.###?.???.###?.???.###?.???.###?.???.###" '(1 1 3 1 1 3 1 1 3 1 1 3 1 1 3))

#+nil
(search-all-arrangements "????" '(1 1))


#+nil
(search-all-arrangements
 "????.#...#...?.????.#...#...?.????.#...#...?.????.#...#...?.????.#...#..."
 '(4 1 1 4 1 1 4 1 1 4 1 1 4 1 1))

#+nil
(let ((SLYNK-STICKERS:*BREAK-ON-STICKERS* (list :after)))
  (search-all-arrangements "????????" '(1 2)))

                                        ; (".??..??...?##.?..??..??...?##.?..??..??...?##.?..??..??...?##.?..??..??...?##."
                                        ;  (1 1 3 1 1 3 1 1 3 1 1 3 1 1 3))
                                        ; ("?#?#?#?#?#?#?#??.?#?#?#?#?#?#?#??.?#?#?#?#?#?#?#??.?#?#?#?#?#?#?#??.?#?#?#?#?#?#?#?"
                                        ;  (1 3 1 6 1 3 1 6 1 3 1 6 1 3 1 6 1 3 1 6))
                                        ; ("????.#...#...?.????.#...#...?.????.#...#...?.????.#...#...?.????.#...#..."
                                        ;  (4 1 1 4 1 1 4 1 1 4 1 1 4 1 1))
                                        ; ("????.######..#####.?.????.######..#####.?.????.######..#####.?.????.######..#####.?.????.######..#####."
                                        ;  (1 6 5 1 6 5 1 6 5 1 6 5 1 6 5))
                                        ; ("?###?????????.?###?????????.?###?????????.?###?????????.?###????????"
                                        ;  (3 2 1 3 2 1 3 2 1 3 2 1 3 2 1)))
