;;;; lambda.infix.asd -*- Mode: Lisp;-*-

(cl:in-package :asdf)

(defsystem :lambda.infix
  :serial t
  :depends-on (:fiveam
               :named-readtables)
  :components ((:file "package")
               (:file "util")
               (:file "lambda.infix")
               (:file "readtable")
               (:file "test")))

(defmethod perform ((o test-op) (c (eql (find-system :lambda.infix))))
  (load-system :lambda.infix)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :lambda.infix.internal :lambda.infix))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))
