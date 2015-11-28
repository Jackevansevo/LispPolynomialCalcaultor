(require "polynomial.lisp")
(in-package  :polynomial-calculator)

(defun report-result (result form)
  (format t "~:[Failed~;Passed~]:  ~a~%" result form))

(defmacro check (&body forms)
  `(progn
     ,@(loop for f in forms collect `(report-result ,f ',f))))

(defun test ()
  (check
    (= (poly- 10 5) 5)
    (= (poly+ 1 2) 3)
    (= (poly* 4 4) 16)))

(test)
