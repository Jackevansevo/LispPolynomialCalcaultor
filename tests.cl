(require "simplyfy.cl")

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form)
  result)

(defmacro check (&body forms)
  `(progn
     ,@(loop for f in forms collect `(report-result ,f ',f))))


;; Tests for (get-sign-value)
(defun test-get-sign-value ()
  (check
    (eql (get-sign-value 'x) 1)
    (eql (get-sign-value '5x) 5)
    (eql (get-sign-value '10x) 10)
    (eql (get-sign-value '200xy) 200)))


;; Tests for (strip-chars)
(defun test-strip-chars ()
  (check
    (string-equal (strip-chars "|xy|" "|") "xy")
    (string-equal (strip-chars "Hello world" "H") "ello world")
    (string-equal (strip-chars "test" "est") "")))


;; Tests for (strip-symbols)
(defun test-strip-symbols ()
  (check
    (string-equal (strip-symbols "5x") "5")
    (string-equal (strip-symbols "5xy") "5")
    (string-equal (strip-symbols "5") "5")
    (string-equal (strip-symbols "xy") "")))


;; Tests for (string-has-integers)
(defun test-string-has-integers ()
  (check
    (eql (string-has-integers "1") T)
    (eql (string-has-integers "2x")T)
    (eql (string-has-integers "20x") T)
    (eql (string-has-integers "x") nil)))


;; Tests for (list-has-integers)
(defun test-list-has-integers ()
  (check
    (eql (list-has-integers '(1 2 3)) T)
    (eql (list-has-integers '(x y 1)) T)
    (eql (list-has-integers '(x y 1)) T)
    (eql (list-has-integers '(x y 50y)) nil)
    (eql (list-has-integers '(x y z)) nil)))


;; Tests for (collect-terms)
(defun test-collect-terms ()
  (check
      (string-equal (collect-terms "x" '(+ x x x) ) "3X")
      (string-equal (collect-terms "x" '(+ x y z) ) "X")
      (string-equal (collect-terms "XX" '(- 2xx xx) ) "XX")
      (string-equal (collect-terms "y" '(- 2y 2y 2y)) "-2y")
      (string-equal (collect-terms "yx" '(+ x 5yx 5yx)) "10yx")
      (string-equal (collect-terms "y" '(- 2y y 5 10)) "Y")
      (string-equal (collect-terms "2x" '(+ x x y)) nil)))


;; Tests for (get-sign)
(defun test-get-sign ()
  (check
    (string-equal (get-sign 'x) "X")
    (string-equal (get-sign 'xy) "XY")
    (string-equal (get-sign '5x) "X")
    (string-equal (get-sign '200xy) "XY")))


;; Tests for (is-operator)
(defun test-is-operator ()
  (check
    (eql (is-operator '+) T)
    (eql (is-operator '-) T)
    (eql (is-operator '*) T)
    (eql (is-operator '20) nil)))


;; All tests are ran below
(format t "Testing has get-sign-value~%")
(test-get-sign-value)
(terpri)

(format t "Testing strip-chars~%")
(test-strip-chars)
(terpri)

(format t "Testing strip-symbols~%")
(test-strip-symbols)
(terpri)

(format t "Testing has-integers~%")
(test-string-has-integers)
(terpri)

(format t "Testing list-has-integers~%")
(test-list-has-integers)
(terpri)

(format t "Testing collect-terms~%")
(test-collect-terms)
(terpri)

(format t "Testing has get-sign~%")
(test-get-sign)
(terpri)

(format t "Testing is-operator~%")
(test-is-operator)
(terpri)
