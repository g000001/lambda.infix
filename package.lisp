;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :lambda.infix
  (:use)
  (:export))

(defpackage :lambda.infix.internal
  (:use :lambda.infix :cl :named-readtables :fiveam)
  (:shadow :multiple-value-bind :time))
