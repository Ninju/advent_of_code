(defpackage :day21/parse
  (:use :cl)
  (:export :parse-module))
(in-package :day21/parse)

(defun parse-module (line)
  (ppcre:register-groups-bind (modifier id outputs)
      ("([%&])?([a-zA-Z]+)\\s*->\\s*(.*)" (string-trim '(#\Space) line))
    (let ((output-ids (ppcre:split "\\s*,\\s*" outputs)))
      (make-instance
       (cond
         ((string= "%" modifier) 'flip-flop)
         ((string= "&" modifier) 'conjunction)
         (t                      'module))
       :id id
       :outputs output-ids))))
