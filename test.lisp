(cl:in-package :lambda.infix.internal)
(in-readtable :lambda.infix)

(def-suite lambda.infix)

(in-suite lambda.infix)

(defmacro x= (x y)
  `(is (equalp ,x ',y)))

(test infix
  (x= (infix-test "3+3+-3*3/3+3◊")
      (+ 3 3 (- (/ (* 3 3) 3)) 3))
  (x= (infix-test "(3+3+-3)*3/3+3◊")
      (+ (/ (* (PROGN (+ 3 3 (- 3))) 3) 3) 3))
  (x= (infix-test "3+3◊")
      (+ 3 3))
  (x= (infix-test "3*3◊")
      (* 3 3))
  (x= (infix-test "3-3-3◊")
      (- 3 3 3))
  (x= (infix-test "(3)◊")
      (PROGN 3))
  (x= (infix-test "if x then y else z◊")
      (IF X Y Z))
  (x= (infix-test "car(x)◊")
      (CAR X))
  (x= (infix-test "x:3◊")
      (SETF X 3))
  (x= (infix-test "a ∈ b◊")
      (MEMBER A B))
  (x= '(progn #◊ 3 * 3◊)
      (PROGN (* 3 3)))
  (x= '(progn #◊ car([1, 2, 3]) ◊)
      (PROGN (CAR (LIST 1 2 3))))
  (x= '(defun fib (n)
        #◊
        if n < 2
          then n
          else fib(n - 1) + fib(n - 2)
        ◊ )
      (DEFUN FIB (N)
        (IF (< N 2)
            N
            (+ (FIB (- N 1)) (FIB (- N 2))))))
  (x= '(let ((x "hello"))
        (dotimes (i (length x))
          #◊
          print(x[i])
          ◊))
      (LET ((X "hello"))
        (DOTIMES (I (LENGTH X)) (PRINT (AREF X I))))))
