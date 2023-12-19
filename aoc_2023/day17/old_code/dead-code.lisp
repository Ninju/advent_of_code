

;; GRAPH STUFF
(let ((graph (make-hash-table :size 100 :test #'equal))
      (lines (lib:read-file-lines (get-inputs-pathname "example.txt"))))

  (loop for line in lines
        from y = 0
        do
           (let ((parsed-line (parse-line line)))
             (loop for heat-loss in parsed-line
                   do
                      (V
