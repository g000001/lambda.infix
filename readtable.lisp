;;;; readtable.lisp

(cl:in-package :lambda.infix.internal)
(in-readtable :common-lisp)

(defun |#◊-reader| (stream char arg)
  (infix-toplevel-parse stream char arg))

(defreadtable :lambda.infix
  (:merge :standard)
  (:dispatch-macro-char #\# #\◊ #'|#◊-reader|)
  (:case :upcase))
