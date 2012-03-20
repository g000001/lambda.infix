(cl:in-package :lambda.infix.internal)

(defmacro defun (name (&rest args) &body body)
  (etypecase name
    ((atom)
     `(cl:defun ,name (,@args) ,@body))
    ((cons)
     (ecase (car name)
       ((:property)
        (destructuring-bind (ignore symbol property)
                            name
          (declare (ignore ignore))
          `(setf (get ',symbol ',property)
                 (lambda (,@args)
                   ,@body))))))))

(defmacro define-site-variable (name &optional val documentation)
  `(defvar ,name ,val ,documentation))

(defmacro select-processor (&body clauses)
  `(progn ,@(cdr (find :x86-64 clauses :key #'car))))


(defmacro defconst (name val &optional documentation)
  `(defparameter ,name ,val ,documentation))

(defmacro deff (name orig)
  `(setf (symbol-function ',name)
         ,(typecase (eval orig)
            (symbol `(symbol-function ,orig))
            (function orig))))

(defmacro multiple-value (vars vals)
  (let* ((ignores '())
         (vars (mapcar (lambda (v)
                         (cond ((string= 'NIL v)
                                (let ((sym (gensym "NIL-")))
                                  (push sym ignores)
                                  sym))
                               ((string= 'IGNORE v)
                                (let ((sym (gensym "IGNORE-")))
                                  (push sym ignores)
                                  sym))
                               (T v)))
                       vars)))
    `(let (,@ignores)
       (cl:multiple-value-setq ,vars ,vals))))

(defun lsh (integer count)
  (ash integer (- count)))

(defun bit-test (x y)
  (logtest x y))

(deff ≤ #'cl:<=)                        ;^\
(deff ≥ #'cl:>=)                        ;^]
(deff ≠ #'cl:/=)                       ;^Z

(deff get-pname #'cl:symbol-name)

(defun getl (plist property-name-list)
  (member-if (lambda (e) (member e property-name-list))
             (symbol-plist plist)))

(defmacro select (item &body cases)
  (let ((v (gensym)))
    `(let ((,v ,item))
       (cond
	 ,@(mapcar (lambda (x)
		     (let ((case (car x))
			   (forms (cdr x)))
		       (cond ((consp case)
			      (if (and (eq 'quote (car case))
				       (eq 't (cadr case)))
				  `('T ,@forms)
				  `((member ,v (list ,@case) :test #'eq) ,@forms)))
			     ((atom case)
			      (if (or (eq t case) (eq 'otherwise case))
				  `('T ,@forms)
				  `((eq ,v ,case) ,@forms)))
			     ('T nil))))
		   cases)))))


;;(getl 'foo '(baz height))
;=>  (BAZ (3 2 1) COLOR BLUE HEIGHT SIX-TWO)

(defmacro condition-case (variables body-form &body clauses)
  "Execute BODY-FORM with conditions handled according to CLAUSES.
Each element of CLAUSES is a clause like those used in CASE.
It specifies one or more condition names, and what to do if they are signalled.

If any of the conditions specified in the clauses happens during BODY-FORM,
it is handled by throwing to this level and executing the matching clause.
Within the clause, the first of VARIABLES is bound to the condition-object
that was signaled.
The values of the last form in the clause are returned from CONDITION-CASE.

If none of the conditions occurs, the values of BODY-FORM are returned
from CONDITION-CASE.

If there is a clause with keyword :NO-ERROR, it is executed after BODY-FORM
if conditions are NOT signaled.  During this clause, the variables VARIABLES
are bound to the values produced by BODY-FORM.  The values of the last form
in the clause are returned from CONDITION-CASE."
  ;; Teco madness.
  ;; (declare (zwei:indentation 1 3 2 1))
  (let ((cond (gensym "Cond-")))
  `(handler-case ,(ensure-no-error-clause clauses
                                          variables
                                          body-form)
     ,@(mapcar (lambda (c)
                 `(,(car c) (,cond)
                    (declare (ignorable ,cond))
                    (let (,@variables)
                      ;;; XXX
                      (setq ,@(mapcan (lambda (v)
                                        (list v cond))
                                      variables))
                      ,@(cdr c))))
         (remove :no-error clauses
                 :key (lambda (x) (and (consp x) (car x))))))))

(defun ensure-no-error-clause (clauses vars form)
  (let ((ne (find :no-error clauses
                  :key (lambda (x) (and (consp x) (car x))))))
    `(multiple-value-bind ,vars ,form
       (declare (ignorable ,@vars))
       ,@(cdr ne))))

(defmacro without-interrupts (&body body)
  `(#+sbcl sb-sys:without-interrupts
    #-sbcl progn
    ,@body))

(defun putprop (object val ind)
  (setf (get object ind) val)
  (get object ind))

(defmacro defprop (p-list val ind)
  `(putprop ',p-list ',val ',ind))


(defun remainder (x y)
  (cl:rem x y))
'WITH-STACK-LIST

(defmacro multiple-value-bind (vars vals &body body)
  (let* ((ignores '())
         (vars (mapcar (lambda (v)
                         (cond ((string= 'NIL v)
                                (let ((sym (gensym "NIL-")))
                                  (push sym ignores)
                                  sym))
                               ((string= 'IGNORE v)
                                (let ((sym (gensym "IGNORE-")))
                                  (push sym ignores)
                                  sym))
                               (T v)))
                       vars)))
    `(cl:multiple-value-bind ,vars ,vals
       ,@(and ignores `((declare (ignore ,@ignores))))
       ,@body)))

(defmacro with-stack-list ((variable &rest elements) &body body)
  `(let ((,variable (list ,@elements)))
     (declare (dynamic-extent ,variable))
     ,@body))

(defmacro with-stack-list* ((variable &rest elements) &body body)
  `(let ((,variable (list* ,@elements)))
     (declare (dynamic-extent ,variable))
     ,@body))


(defmacro defsubst (name (&rest args) &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name (,@args) ,@body)))

(deff string-length #'cl:length)
(deff time #'cl:get-internal-real-time)
(deff time-difference #'cl:-)

(defun time-increment (time interval)
  (+ time interval))

(declaim (inline delq))
(defun delq (item list)
  (delete item list :test #'eq))

(declaim (inline memq))
(defun memq (item list)
  (member item list :test #'eq))

(declaim (inline assq))
(defun assq (item list)
  (assoc item list :test #'eq))

(deff SUBSTRING #'subseq)
(deff NSUBSTRING #'subseq)

#|(defun barf (fmt &rest args)
  (apply #'error fmt args))|#

(defun ass (pred data alist)
  (assoc data alist :test pred))

(defun neq (x y)
  (not (eq x y)))

(defun fixp (obj)
  (integerp obj))

(defun rest1 (list)
  (nthcdr 1 list ))

(defmacro selectq (item &body cases)
  (let ((v (gensym)))
    `(let ((,v ,item))
       (cond
	 ,@(mapcar (lambda (x)
		     (let ((case (car x))
			   (forms (cdr x)))
		       (cond ((consp case)
			      (if (equal '(quote t) case)
				  `('T ,@forms)
				  `((member ,v ',case :test #'eq) ,@forms)))
			     ((atom case)
			      (if (or (eq t case) (eq 'otherwise case))
				  `('T ,@forms)
				  `((eq ,v ',case) ,@forms)))
			     ('T nil))))
		   cases)))))

(defun si>princ-function (obj &optional (stream *standard-output*))
  (princ obj stream))


(defun \\ (x y)
  (rem x y))

(defun fix (n)
  (values (floor n)))

(defun symeval (sym)
  (symbol-value sym))

(defun ferror (stream fmt &rest args)
  (declare (ignore stream))
  (apply #'error fmt args))

(DEFUN STRING-SEARCH (KEY STRING &OPTIONAL (FROM 0) TO (KEY-FROM 0) KEY-TO
                          &AUX KEY-LEN )
  "Returns the index in STRING of the first occurrence of KEY past FROM, or NIL.
If TO is non-NIL, the search stops there, and the value is NIL
if no occurrence of KEY is found before there.
KEY-FROM and KEY-TO may be used to specify searching for just a substring of KEY.
CONSIDER-CASE if non-NIL means we distinguish letters by case."
  (let ((string (string STRING))
        (key   (string KEY)) ) ;??
    (UNLESS KEY-TO
      (SETQ KEY-TO (LENGTH KEY)) )
    (SETQ KEY-LEN (- KEY-TO KEY-FROM))
    (OR TO (SETQ TO (LENGTH STRING)))
    (COND ((= KEY-FROM KEY-TO)
           (AND (≤ FROM TO) FROM) )
          (T
           (SETQ TO (1+ (- TO KEY-LEN))) ;Last position at which key may start + 1
           (PROG (CH1)
                 (WHEN (MINUSP TO) (RETURN NIL))
                 (SETQ CH1 (CHAR KEY KEY-FROM))
              LOOP ;Find next place key might start
                 (OR (SETQ FROM (position CH1 STRING :start FROM :end TO))
                     ;;********************
                     (RETURN NIL) )
                 (AND (STRING-EQUAL KEY STRING
                                    :start1 KEY-FROM
                                    :start2 FROM
                                    :end2 (+ from KEY-LEN))
                      ;;********************
                      (RETURN FROM) )
                 (INCF FROM) ;Avoid infinite loop.  %STRING-SEARCH-CHAR
                 (GO LOOP) ))))) ;  does right thing if from ^] to.



(DEFUN STRING-SEARCH-NOT-SET (CHAR-SET STRING &OPTIONAL (FROM 0) TO CONSIDER-CASE)
  "Returns the index in STRING of the first char past FROM that's NOT in CHAR-SET, or NIL.
CHAR-SET can be a list of characters or a string.
If TO is non-NIL, the search stops there, and the value is NIL
if no occurrence of a char not in CHAR-SET is found before there.
Case matters during character comparison if CONSIDER-CASE is non-NIL."
  (CTYPECASE CHAR-SET
    ((OR CHARACTER FIXNUM)
      (STRING-SEARCH-NOT-CHAR CHAR-SET STRING FROM TO CONSIDER-CASE) )
    (SEQUENCE
      (IF (NULL CHAR-SET)
         NIL
         (let ((string string))
           (OR TO (SETQ TO (LENGTH STRING)))
           (DO ((I FROM (1+ I))
                (FUN (IF (CL:LISTP CHAR-SET) #'MEM #'ARRAY-MEM)) )
               ((≥ I TO) NIL)
             (OR (IF CONSIDER-CASE
                     (FUNCALL FUN #'CHAR= (CHAR STRING I) CHAR-SET)
                     (FUNCALL FUN #'CHAR-EQUAL (CHAR STRING I) CHAR-SET) )
                 (RETURN I) )))))))


(DEFUN STRING-SEARCH-SET (CHAR-SET STRING &OPTIONAL (FROM 0) TO CONSIDER-CASE)
  "Returns the index in STRING of the first char past FROM that's in CHAR-SET, or NIL.
CHAR-SET can be a list of characters or a string.
If TO is non-NIL, the search stops there, and the value is NIL
if no occurrence of a char in CHAR-SET is found before there.
Case matters during character comparison if CONSIDER-CASE is non-NIL."
  (CTYPECASE CHAR-SET
    ((OR CHARACTER FIXNUM)
     (STRING-SEARCH-CHAR CHAR-SET STRING FROM TO CONSIDER-CASE) )
    (SEQUENCE
     (IF (NULL CHAR-SET)
         NIL
         (progn
           (OR TO (SETQ TO (LENGTH STRING)))
           (DO ((I FROM (1+ I))
                (FUN (IF (CL:LISTP CHAR-SET) #'MEM #'ARRAY-MEM)) )
               ((≥ I TO) NIL)
             (AND (IF CONSIDER-CASE
                      (FUNCALL FUN #'CHAR= (CHAR STRING I) CHAR-SET)
                      (FUNCALL FUN #'CHAR-EQUAL (CHAR STRING I) CHAR-SET) )
                  (RETURN I) )))))))


(defun find-position-in-list (item list)
  (position item list))

(defun parse-number (string &optional (start 0) end)
  (the number
    (read-from-string string nil nil :start start :end end)))

(defun string-search-not-char (char string
                               &optional (start 0) end consider-case)
  (let ((= (if consider-case #'char= #'char-equal)))
    (position char string :start start :end end :test-not =)))

(defun string-search-char (char string
                           &optional (start 0) end consider-case)
  (let ((= (if consider-case #'char= #'char-equal)))
    (position char string :start start :end end :test =)))


(defun mem (pred item list)
  (member item list :test pred))

(defun array-mem (pred item array)
  (find item array :test pred))

(defun minus (num)
  (- num))

;;; eof
