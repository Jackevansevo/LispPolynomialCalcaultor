(require "polynomial.cl")
(require "simplyfy.cl")

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form)
  result)

(defmacro check (&body forms)
  `(progn
     ,@(loop for f in forms collect `(report-result ,f ',f))))


(defun test-collect-terms ()
  (check
      (string-equal (collect-terms 'x '(+ x x x) ) "3X")
      (string-equal (collect-terms 'x '(+ x y z) ) "1X")
      (string-equal (collect-terms 'y '(- 2y 2y 2y)) "-2y")
      (string-equal (collect-terms 'yx '(+ x 5yx 5yx)) "10yx")
      (string-equal (collect-terms '20x '(+ x x y)) nil)))

(format t "Testing collect-terms~%")
(test-collect-terms)
(terpri)

(defun test-has-integers ()
  (check
    (eql (has-integers "1") T)
    (eql (has-integers "2x")T)
    (eql (has-integers "20x") T)
    (eql (has-integers "x") Nil)))

(format t "Testing has integers~%")
(test-has-integers)
(terpri)

(defun test-get-sign ()
  (check
    (string-equal (get-sign 'x) "X")
    (string-equal (get-sign 'xy) "XY")
    (string-equal (get-sign '5x) "X")
    (string-equal (get-sign '200xy) "XY")))

(format t "Testing has get-sign~%")
(test-get-sign)
(terpri)


(defun test-get-sign-value ()
  (check
    (eql (get-sign-value 'x) 1)
    (eql (get-sign-value '5x) 5)
    (eql (get-sign-value '10x) 10)
    (eql (get-sign-value '200xy) 200)))

(format t "Testing has get-sign-value~%")
(test-get-sign-value)
(terpri)

