;;;; lambda.infix.lisp

(cl:in-package :lambda.infix.internal)

;;; "lambda.infix" goes here. Hacks and glory await!

;;;-*- Mode:LISP; Package:SI; Fonts:(CPTFONT TR12 TR12I); Base:8; readtable: ZL -*-


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                      ;;;
;;;     Based on a theory of parsing presented in:                       ;;;
;;;                                                                      ;;;
;;;         Pratt, Vaughan R., ``Top Down Operator Precedence,''         ;;;
;;;         ACM Symposium on Principles of Programming Languages         ;;;
;;;         Boston, MA; October, 1973.                                   ;;;
;;;                                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; On this page is the tokenizer.  The next page is the parser.
;;; After that come the operator definitions.

;;; Macros and functions used by the tokenizer loop.

#|(DEFUN INFIX-TYI ()
  (MULTIPLE-VALUE-BIND (NIL NIL CH)
      ;; (XR-XRTYI *STANDARD-INPUT* NIL T)
      (read-char *STANDARD-INPUT*)
    CH))|#
(DEFUN INFIX-TYI (&optional (stream *STANDARD-INPUT*))
  (read-char stream nil nil))

#|(DEFSUBST INFIX-UNTYI (CH) (SEND *STANDARD-INPUT* ':UNTYI CH))|#

(DEFSUBST INFIX-UNTYI (CH &optional (stream *standard-input*))
  (unread-char ch stream))

#|(DEFUN INFIX-TYIPEEK ()
  (LET ((CH (INFIX-TYI)))
    (PROG1 CH (SEND *STANDARD-INPUT* ':UNTYI CH))))|#
(DEFUN INFIX-TYIPEEK (&optional (stream *standard-input*))
  (peek-char nil stream nil nil))

;; (defvar PREVIOUS-LOOKUP nil)
;; (defvar NUMBER-SO-FAR nil)

(DEFMACRO INFIX-RETURN-TOKEN (C STRING stream)
  `(PROGN (IF C (INFIX-UNTYI ,C ,stream))
          (RETURN (IF NUMBER-SO-FAR (READ-FROM-STRING ,STRING)
                    (IF PREVIOUS-LOOKUP (CDR PREVIOUS-LOOKUP)
                      (INTERN STRING))))))

(DEFSUBST INFIX-WHITESPACEP (C)
  (MEMQ C '(#\Space #\RETURN #\LINEFEED #\TAB #\PAGE)))

(DEFUN INFIX-DIGITP (X) (NOT (OR (< X (char-code #\0)) (> X (char-code #\9)))))

;;;; First step: The tokenizer.

(DEFVAR INFIX-TOKEN-TABLE NIL)
;;; The tokenizer is a simple loop with the character TYI'd pushed on the
;;; token buffer after a series of special cases are checked.
;;; The boolean state variables could be replaced with predicates
;;; that look back on what in in the buffer, however the present implementation
;;; is highly straightforward.


(DEFUN INFIX-READ-TOKEN (&optional (stream *standard-input*))
  (DO ((C (INFIX-SKIP-WHITESPACE stream) (INFIX-TYI stream))
       (STRING (MAKE-ARRAY #o40 ':ELEMENT-TYPE 'character ':FILL-POINTER 0))
       TEM
       (THIS-QUOTED-P NIL NIL)
       (QUOTED-P NIL)
       (NUMBER-SO-FAR T)
       (PREVIOUS-LOOKUP NIL)
       )
      (())
    (COND ((NULL C)
           (IF (EQUAL STRING "")
               (PROGN
                 #|(CERROR ':NO-ACTION NIL
                         'SYS:READ-END-OF-FILE
                         "End of file on ~S within infix expression."
                         *STANDARD-INPUT*)|#
                 (cerror "End of file on stream within infix expression."
                         "=>")
                 (RETURN '◊))
               (RETURN
                 (IF NUMBER-SO-FAR
                     (READ-FROM-STRING STRING)
                     (IF PREVIOUS-LOOKUP
                         (CDR PREVIOUS-LOOKUP)
                         (INTERN STRING))))))
          ((OR (INFIX-WHITESPACEP C)
               (EQ C #\◊))
           (IF (EQUAL STRING "")
               (RETURN '◊)
               (INFIX-RETURN-TOKEN C STRING stream)))
          ((char= C #\!)
           (IF (EQUAL STRING "")
               (PROGN (INFIX-UNTYI #\! stream)
                      (RETURN '\!))
               (INFIX-RETURN-TOKEN C STRING stream)))
          ((char= C #\\)
           (SETQ QUOTED-P T THIS-QUOTED-P T NUMBER-SO-FAR NIL)
           (SETQ C (INFIX-TYI stream)))
          ((char= C #\")
           (IF (EQUAL STRING "")
               (PROGN (INFIX-UNTYI C stream)
                      (RETURN (READ stream)))
             (INFIX-RETURN-TOKEN C STRING stream))))
    (VECTOR-PUSH-EXTEND (IF THIS-QUOTED-P C (CHAR-UPCASE C))
                       STRING)
    (WHEN NUMBER-SO-FAR
      (UNLESS (INFIX-NUMBER-TOKEN-P STRING)
        (COND ((char= (CHAR-UPCASE C) #\E)
               (IF (EQ NUMBER-SO-FAR T)
                   (SETQ NUMBER-SO-FAR 'E)
                 (SETQ NUMBER-SO-FAR NIL)))
              ((char<= #\A (CHAR-UPCASE C) #\Z)
               (SETQ NUMBER-SO-FAR NIL))
              ((AND (MEMBER C '(#\+ #\-))
                    (EQ NUMBER-SO-FAR 'E))
               (SETQ NUMBER-SO-FAR 'ESIGN))
              ((EQUAL STRING ".")
               (IF (INFIX-DIGITP (INFIX-TYIPEEK stream))
                   (SETQ NUMBER-SO-FAR T)
                 (RETURN '\.)))
              ((= (LENGTH STRING) 1)
               (SETQ NUMBER-SO-FAR NIL))
              (T
               (VECTOR-POP STRING)
               (INFIX-RETURN-TOKEN C STRING stream)))))
    (WHEN (AND (NOT PREVIOUS-LOOKUP)
               (NOT NUMBER-SO-FAR)
               (NOT THIS-QUOTED-P)
               (≠ (LENGTH STRING) 1)
               (DOLIST (ELT INFIX-TOKEN-TABLE)
                 (WHEN (char= (AREF (CAR ELT) 0) C)
                   (RETURN T))))
      (VECTOR-POP STRING)
      (INFIX-RETURN-TOKEN C STRING stream))
    (UNLESS QUOTED-P
      (SETQ TEM NIL)
      (DOLIST (ELT INFIX-TOKEN-TABLE)
        (IF (STRING-EQUAL (CAR ELT) STRING
                          :start1 0
                          :start2 0
                          :end1 (if (<= (length (car elt))
                                        (LENGTH STRING))
                                    (length (car elt))
                                    (LENGTH STRING)))
            (RETURN (SETQ TEM ELT))))
      (AND (NULL TEM) PREVIOUS-LOOKUP
           (PROGN (INFIX-UNTYI C stream) (RETURN (CDR PREVIOUS-LOOKUP))))
      (SETQ PREVIOUS-LOOKUP TEM))))

;;; Skip past whitespace and comments.
;;; Return the first nonwhite charater not in a comment.
(DEFUN INFIX-SKIP-WHITESPACE (stream)
  (DO ((COMMENTP ())(C))
      (())
    (SETQ C (INFIX-TYI stream))
    (COND ((NULL C) (RETURN C))
          ((char= C #\%)
           (SETQ COMMENTP (NOT COMMENTP)))
          ((INFIX-WHITESPACEP C))
          ((NOT COMMENTP)
           (RETURN C)))))

;;; Make an entry for TOKEN (a symbol) in our token-table.
(DEFUN INFIX-PUTTOK (TOKEN &AUX (STRING (GET-PNAME TOKEN)) LETTERS NONLETTERS)
  (DOTIMES (I (LENGTH STRING))
    (IF (char<= #\A (AREF STRING I) #\Z)
        (SETQ LETTERS T)
        (SETQ NONLETTERS T)))
  (IF LETTERS
      (WHEN NONLETTERS (FERROR NIL "Invalid infix token ~S defined." TOKEN))
      (UNLESS (ASSOC (GET-PNAME TOKEN) INFIX-TOKEN-TABLE)
        (PUSH (CONS (GET-PNAME TOKEN) TOKEN) INFIX-TOKEN-TABLE))))

(DEFUN INFIX-NUMBER-TOKEN-P (STRING)
  ;; its more efficient to determine the type of
  ;; the token by collecting information in state variables
  ;; as it is read. However we aren't that sure of our book-keeping.
  ;; This way we accept whatever the reader does.
  (IGNORE-ERRORS
    (MULTIPLE-VALUE-BIND (VALUE END-POS)
        (READ-FROM-STRING STRING)
      (AND (NUMBERP VALUE)
           (>= END-POS (LENGTH STRING))))))

;;;; The actual parser.

(DEFVAR INFIX-TOKEN NIL
  "The token waiting to be examined, in parsing an infix expression.")

(DEFVAR INFIX-LAST-TOKEN NIL
  "While invoking a token's handlers, this is that token.
INFIX-TOKEN will be the following token.")

(DEFCONST INFIX-NUDL '(INFIX-START-EXP-FUNCTION))
(DEFCONST INFIX-LEDL '(INFIX-CONTINUE-EXP-FUNCTION))
(DEFCONST INFIX-LBPL '(INFIX-LEFT-BINDING-POWER))

#|(DEFUN INFIX-TOPLEVEL-PARSE (*STANDARD-INPUT* IGNORE IGNORE)
  (LET ((INFIX-TOKEN (INFIX-READ-TOKEN)))
    (INFIX-PARSE -1)))|#

(DEFUN INFIX-TOPLEVEL-PARSE (stream IGN ORE)
  (declare (ignore ign ore))
  (LET ((INFIX-TOKEN (INFIX-READ-TOKEN stream)))
    (INFIX-PARSE -1 stream)))

(DEFUN INFIX-TEST (STRING)
  (WITH-INPUT-FROM-STRING (*STANDARD-INPUT* STRING)
    (LET ((INFIX-TOKEN (INFIX-READ-TOKEN)))
      (INFIX-PARSE -1 *standard-input*))))

(DEFUN INFIX-PARSE (RIGHT-BINDING-POWER &optional (stream *standard-input*))
  "Reads and returns one expression, using right binding power RIGHT-BINDING-POWER.
Encountering a token whose left binding power is ≥ RIGHT-BINDING-POWER
causes us to return, leaving that token ungobbled."
  (DO ((TRANSLATION
         ;; Process a token that begins an expression (or should do so).
         (LET ((START-EXP-FUNCTION (INFIX-GETDEN INFIX-NUDL))
               (LEFT-BINDING-POWER (INFIX-GETDEN INFIX-LBPL)))
           (IF START-EXP-FUNCTION
               (PROGN
                 (SETQ INFIX-TOKEN (INFIX-READ-TOKEN stream))
                 (FUNCALL START-EXP-FUNCTION stream))
             (IF LEFT-BINDING-POWER
                 (PROGN
                   #|(CERROR ':NO-ACTION NIL 'SYS:READ-ERROR-1
                           "Missing token in infix expression before \"~A\"."
                           INFIX-TOKEN)|#
                   (warn "Missing token in infix expression before \"~A\"."
                         INFIX-TOKEN)
                   (RETURN NIL))
               (PROG1 INFIX-TOKEN
                      (SETQ INFIX-TOKEN (INFIX-READ-TOKEN stream))))))
         ;; Process a token that extends an expression.
         (LET ((CONTINUE-EXP-FUNCTION (INFIX-GETDEN INFIX-LEDL)))
           (IF (NULL CONTINUE-EXP-FUNCTION)
               (PROGN
                 #|(CERROR ':NO-ACTION NIL 'SYS:READ-ERROR-1
                         "\"~A\" with left-argument in infix expression."
                         INFIX-TOKEN)|#
                 (warn "\"~A\" with left-argument in infix expression."
                         INFIX-TOKEN)
                 (SETQ INFIX-TOKEN (INFIX-READ-TOKEN stream))
                 TRANSLATION)                   ;Ignore this token.
               (progn
                 (SETQ INFIX-TOKEN (INFIX-READ-TOKEN stream))
                 (FUNCALL CONTINUE-EXP-FUNCTION TRANSLATION stream))))))
      (())
    (WHEN (≥ RIGHT-BINDING-POWER (OR (INFIX-GETDEN INFIX-LBPL) 0))
      (RETURN TRANSLATION))))

(DEFUN INFIX-GETDEN (INDL)
  (AND (SYMBOLP INFIX-TOKEN)
       (CADR (GETL INFIX-TOKEN INDL))))

(DEFUN INFIX-PARSE-LIST (RIGHT-BINDING-POWER SEPARATOR-TOKEN END-TOKEN
                                             &optional (stream *standard-input*))
  "Reads a list of expressions, using RIGHT-BINDING-POWER for each one.
We expect expressions in the list to be separated by tokens EQ to SEPARATOR-TOKEN
and the list to be terminated by a token EQ to END-TOKEN.
The END-TOKEN is gobbled.
If END-TOKEN is NIL, we accept any ending token and don't gobble it."
  (DO (ACCUM
       (FIRST T NIL))
      (())
    (SELECT INFIX-TOKEN
      (SEPARATOR-TOKEN
       (SETQ INFIX-TOKEN (INFIX-READ-TOKEN stream)))
      (END-TOKEN
       (SETQ INFIX-TOKEN (INFIX-READ-TOKEN stream))
       (RETURN (NREVERSE ACCUM)))
      (T (UNLESS FIRST
           (IF END-TOKEN
               #|(CERROR ':NO-ACTION NIL 'SYS:READ-ERROR-1
                       "\"~A\" read in infix expression where \"~A\" or \"~A\" was expected."
                       INFIX-TOKEN SEPARATOR-TOKEN END-TOKEN)|#
               (warn "\"~A\" read in infix expression where \"~A\" or \"~A\" was expected."
                       INFIX-TOKEN SEPARATOR-TOKEN END-TOKEN))
           (RETURN (NREVERSE ACCUM)))))
    (PUSH (INFIX-PARSE RIGHT-BINDING-POWER stream) ACCUM)))

;;;; Operator-defining macros.

(DEFMACRO DEFINFIX (TOKEN LEFT-BINDING-POWER (ARG stream) &BODY BODY)
  "Define TOKEN (a symbol) as an infix operator for infix expression parsing.
LEFT-BINDING-POWER is a measure of precedence on the left.
ARG is a symbol used in BODY to refer to the expression on the left.
BODY should parse the arguments on the right
and return the expression that contains this operation.
Example:
   (DEFINFIX + 50 (LEFT)
     `(+ ,LEFT ,(INFIX-PARSE 51)))
51 is used within to make it left-associative.
47 would be right-associative."
  `(PROGN (INFIX-PUTTOK ',TOKEN)
          (DEFPROP ,TOKEN ,LEFT-BINDING-POWER INFIX-LEFT-BINDING-POWER)
          (zl:DEFUN (:property ,TOKEN INFIX-CONTINUE-EXP-FUNCTION) (,ARG ,stream)
            . ,BODY)))

(DEFMACRO DEFPREFIX (TOKEN (stream) &BODY BODY)
  "Define TOKEN (a symbol) as an prefix operator for infix expression parsing.
BODY should parse the arguments on the right
and return the expression that contains this operation.
Example:
   (DEFPREFIX - `(- ,(INFIX-PARSE 1000)))"
  `(PROGN (INFIX-PUTTOK ',TOKEN)
          (zl:DEFUN (:property ,TOKEN INFIX-START-EXP-FUNCTION) (,stream)
            . ,BODY)))

(DEFMACRO DEFDELIMITER (TOKEN)
  "Define TOKEN (a symbol) as a delimiter token for infix expression parsing.
This token has no syntax assigned to it; other tokens' definitions
will check explicitly for encountering this token.
This is used for comma and close parenthesis."
  `(PROGN (INFIX-PUTTOK ',TOKEN)
          (DEFPROP ,TOKEN 0 INFIX-LEFT-BINDING-POWER)))

(DEFMACRO DEFREPINFIX (TOKEN LEFT-BINDING-POWER
                       &OPTIONAL (RIGHT-BINDING-POWER LEFT-BINDING-POWER)
                       (FUNCTION TOKEN))
  "Define TOKEN as a multi-argument infix operator for infix expression parsing.
TOKEN is also used by default as the function for the expression to call,
 unless you override that by specifying a FUNCTION.
Example:   (DEFREPINFIX + 50) makes A+B+C parse into (+ A B C).
RIGHT-BINDING-POWER better be greater than or equal to LEFT-BINDING-POWER;
it defaults to be equal."
  `(DEFINFIX ,TOKEN ,LEFT-BINDING-POWER (LEFT stream)
     `(,',FUNCTION ,LEFT . ,(INFIX-PARSE-LIST ,RIGHT-BINDING-POWER ',TOKEN NIL
                                              stream))))

;;;; Definitions of operators.

(DEFPROP ◊ -1 INFIX-LEFT-BINDING-POWER)

#|(DEFUN (\( INFIX-START-EXP-FUNCTION) ()
  `(PROGN . ,(INFIX-PARSE-LIST 0 '\, '\))))|#

(zl:DEFUN (:property \( INFIX-START-EXP-FUNCTION) (stream)
  `(PROGN . ,(INFIX-PARSE-LIST 0 '\, '\) stream)))

(DEFINFIX \[ 200. (LEFT stream)
  `(AREF ,LEFT . ,(INFIX-PARSE-LIST 0 '\, '\] stream)))

#|(DEFUN (\[ INFIX-START-EXP-FUNCTION) ()
  `(LIST . ,(INFIX-PARSE-LIST 0 '\, '\])))|#

(zl:DEFUN (:property \[ INFIX-START-EXP-FUNCTION) (stream)
  `(LIST . ,(INFIX-PARSE-LIST 0 '\, '\] stream)))

(DEFINFIX \( 200. (LEFT stream)
  `(,LEFT . ,(INFIX-PARSE-LIST 0 '\, '\) stream)))

(DEFDELIMITER \))
(DEFDELIMITER \,)
(DEFDELIMITER \])

(DEFINFIX \: 180. (LEFT stream)
  `(SETF ,LEFT ,(INFIX-PARSE 20. stream)))

(DEFINFIX \^ 140. (LEFT stream)
  `(^ ,LEFT ,(INFIX-PARSE 139. stream)))

(DEFREPINFIX * 120.)
(DEFREPINFIX / 120.)
;; (DEFREPINFIX CLI:\ 120.)

(DEFREPINFIX + 100.)
(DEFREPINFIX - 100.)
(DEFPREFIX - (stream) `(- ,(INFIX-PARSE 100. stream)))
(DEFPREFIX + (stream) (INFIX-PARSE 100. stream))

(DEFINFIX \. 95. (LEFT stream)
  (LET ((RIGHT (INFIX-PARSE 94. stream)))
    (IF (AND (CONSP RIGHT)
             (EQ (CAR RIGHT) 'LIST*))
        `(LIST* ,LEFT . ,(CDR RIGHT))
      `(LIST* ,LEFT ,RIGHT))))

(DEFINFIX \@ 95. (LEFT stream)
  (LET ((RIGHT (INFIX-PARSE 94. stream)))
    (IF (AND (CONSP RIGHT)
             (EQ (CAR RIGHT) 'APPEND))
        `(APPEND ,LEFT . ,(CDR RIGHT))
      `(APPEND ,LEFT ,RIGHT))))

(DEFINFIX \∈ 80. (LEFT stream)
  `(MEMBER ,LEFT ,(INFIX-PARSE 79. stream)))

(DEFREPINFIX < 80.)
(DEFREPINFIX > 80.)
(DEFREPINFIX = 80.)
(DEFREPINFIX ≥ 80.)
(DEFREPINFIX ≤ 80.)
(DEFREPINFIX ≠ 80.)

(DEFPREFIX NOT (stream)
  `(NOT ,(INFIX-PARSE 70. stream)))

(DEFREPINFIX AND 60.)
(DEFREPINFIX OR 50.)

;; RBP of ":" (assignment) is 20.

(DEFDELIMITER THEN)
(DEFDELIMITER ELSE)

(DEFPREFIX IF (stream) NIL
  (LET ((COND-FORM (INFIX-PARSE 45. stream)))
    (IF (EQ INFIX-TOKEN 'THEN)
        (SETQ INFIX-TOKEN (INFIX-READ-TOKEN stream))
      #|(CERROR ':NO-ACTION NIL 'SYS:READ-ERROR-1 "No THEN in an infix IF expression.")|#
        (warn "No THEN in an infix IF expression."))
    (LET ((THEN-FORMS (INFIX-PARSE-LIST 25. '\, NIL stream))
          ELSE-FORMS)
      (WHEN (EQ INFIX-TOKEN 'ELSE)
         (SETQ INFIX-TOKEN (INFIX-READ-TOKEN stream))
         (SETQ ELSE-FORMS (INFIX-PARSE-LIST 25. '\, NIL stream)))
      (COND ((NULL ELSE-FORMS)
             `(WHEN ,COND-FORM . ,THEN-FORMS))
            ((NULL THEN-FORMS)
             `(UNLESS ,COND-FORM . ,ELSE-FORMS))
            ((NULL (CDR THEN-FORMS))
             `(IF ,COND-FORM ,(CAR THEN-FORMS) . ,ELSE-FORMS))
            (T
             `(IF ,COND-FORM (PROGN . ,THEN-FORMS) . ,ELSE-FORMS))))))

(DEFPREFIX \! (stream) NIL
  ;; Reading ! as a token UNTYI's it.  So flush the UNTYI'd one.
  (INFIX-TYI)
  (PROG1 (READ stream)
         ;; Read in the token that follows the !'d s-expression.
         (SETQ INFIX-TOKEN (INFIX-READ-TOKEN stream))))
#||||#
