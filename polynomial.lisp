;;;; Aims to implement simple polynomial arithmetic in Lisp
;;;; Follows (for the most part) functional programming practices
;;;; Assumes all inputs are valid ಠ‿ಠ

;;;; PACKAGE DECLARATIONS
(defpackage :polynomial-calculator
  (:use :common-lisp)
  (:export :polly+
           :polly-
           :poll*))

(in-package :polynomial-calculator)

;;;; ARITHMETIC FUNCTIONS
(defun poly+ (p1 p2)
  (format t "Called adder function with arguments ~d and ~d ~%" p1 p2))

(defun poly- (p1 p2)
  (format t "Called subtraction function with arguments ~d and ~d ~%" p1 p2))

(defun poly* (p1 p2)
  (format t "Called multiplication function with arguments ~d and ~d ~%" p1 p2))

;; Create interactive prompt to enter data
(defun prompt-read (prompt)
  ;; Code inspired by: gigamonkeys.com/book/practical-a-simple-database.html
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

;; Prompts the user to enter in a polynomial
(defun prompt-user-input ()
  (return-from prompt-user-input 
               (list
                 (prompt-read "Operation [+/-/*]")
                 (prompt-read "Polynomial")
                 (prompt-read "Polynomial"))))

;; Collects the terms inputted by the user
(defun collect-inputs (args)
  ;; Check to see if args have been passed
  (if (> (length args) 0)
      (return-from collect-inputs (car args))
    (progn
      (format t "Arguments not passed~%")
      (if (not (y-or-n-p "Choice:"))
        ;; If they choose not to enter input then exit the program
        (progn
          (format t "~%Farewell!~%")
          (ext:exit))
        ;; Else return inputs collected from teh user
        (return-from collect-inputs(prompt-user-input))))))

;; Returns which arithmetic function need to be called
(defun get-arithmetic-function (input)
  ;; Figure out what arithmetic function we need to call
  (if (eql (car input) '+)
      (return-from get-arithmetic-function #'poly+))
  (if (eql (car input) '-)
    (return-from get-arithmetic-function #'poly-))
  (if (eql (car input) '*)
    (return-from get-arithmetic-function #'poly*)))

;; Main function
(defun main (&rest args)
  ;; Gets the user input and proceses it
  (let ((input (collect-inputs args)))
    ;; Get arithmetic symbol at the start of 
    (funcall (get-arithmetic-function input) (nth 1 input) (nth 2 input))))

;; Calls main function
; (main '(+ (+ x 2) (+ y 3)))
(main)
