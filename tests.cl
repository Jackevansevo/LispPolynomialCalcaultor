(require "simplyfy.cl")

(defun report-result (result form)
  (format t "~:[~c[31mFAIL~;~c[32mPASS~] ~c[0m ... ~a~%" result #\ESC #\ESC form)
  result)

(defmacro check (&body forms)
  `(progn
     ,@(loop for f in forms collect `(report-result ,f ',f))))


;; Tests for (strip-chars)
(defun test-strip-chars ()
  (check
    (string-equal (strip-chars "1234567890|" '+) '+)
    (string-equal (strip-chars "1234567890|" 'x) 'x)
    (string-equal (strip-chars "1234567890|" '100y) 'y)
    (string-equal (strip-chars "1234567890|" '200xy) 'xy)
    (string-equal (strip-chars "1234567890|" 'y) 'y)))


;; Tests for (is-operator)
(defun test-is-operator ()
  (check
    (eql (is-operator '+) T)
    (eql (is-operator '-) T)
    (eql (is-operator '*) T)
    (eql (is-operator '20) nil)))

;; Tests for (sign-value)
(defun test-sign-value ()
  (check
    (eql (sign-value 'x) 1)
    (eql (sign-value '5x) 5)
    (eql (sign-value '10x) 10)
    (eql (sign-value '200xy) 200)
    (eql (sign-value '5) 5)))

;; Tests for (sign-of)
(defun test-sign-of ()
  (check
    (string-equal (sign-of 'x) "X")
    (string-equal (sign-of 'xy) "XY")
    (string-equal (sign-of '5x) "X")
    (string-equal (sign-of '200xy) "XY")))

;; Tests for (has-integers?)
(defun test-has-integers? ()
  (check
    (eql (has-integers? '+) nil)
    (eql (has-integers? 'x) nil)
    (eql (has-integers? '5x) T)
    (eql (has-integers? '100xy) T)
    (eql (has-integers? '-) nil)))

;; Tests for (list-has-integers?)
(defun test-list-has-integers? ()
  (check
    (eql (list-has-integers? '(1 2 3)) T)
    (eql (list-has-integers? '(x y 1)) T)
    (eql (list-has-integers? '(x y 1)) T)
    (eql (list-has-integers? '(x y 50y)) nil)
    (eql (list-has-integers? '(x y z)) nil)))

;; Tests for (test-is-in-list)
(defun test-is-in-list? ()
  (check
    (eql (is-in-list? 1 '(1 2 3)) T)
    (eql (is-in-list? '5xy '(5xy 2 3)) T)
    (eql (is-in-list? 0 '(1 2 3)) nil)
    (eql (is-in-list? '+ '(+ 1 2 3)) T)))

;; Tests for (collect-terms)
(defun test-collect-terms ()
  (check
      (eql (collect-terms 'x '(+ x x x) ) '3x)
      (eql (collect-terms 'x '(+ x y z) ) 'x)
      (eql (collect-terms 'xx '(- 2xx xx) ) 'xx)
      (eql (collect-terms 'y  '(- 2y 2y 2y)) '-2y)
      (eql (collect-terms 'yx '(+ x 5yx 5yx)) '10yx)
      (eql (collect-terms 'y '(- 2y y 5 10)) 'y)
      (eql (collect-terms '2x '(+ x x y)) nil)))

;; Tets for (collect-integers)
(defun test-collect-integers ()
  (check
    (eql (collect-integers '(+ 1 x 1 y 1 z)) 3)
    (eql (collect-integers '(+ 1 2 3 4 5 6)) 21)
    (eql (collect-integers '(+ x y z)) 0)))


(format t "Testing has strip-chars~%")
(test-strip-chars)
(terpri)

(format t "Testing is-operator~%")
(test-is-operator)
(terpri)

(format t "Testing has sign-value~%")
(test-sign-value)
(terpri)

(format t "Testing has sign-of~%")
(test-sign-of)
(terpri)

(format t "Testing has-integers?~%")
(test-has-integers?)
(terpri)

(format t "Testing list-has-integers?~%")
(test-list-has-integers?)
(terpri)

(format t "Testing is-in-list?~%")
(test-is-in-list?)
(terpri)

(format t "Testing collect-terms~%")
(test-collect-terms)
(terpri)

(format t "Testing collect-integers~%")
(test-collect-integers)
(terpri)
