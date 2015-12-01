:;; PACKAGE DECLARATIONS
(defpackage :polynomial-calculator
  (:use :common-lisp)
  (:export :poly+
           :poly-
           :pol*))

(in-package :polynomial-calculator)


; ███████╗██╗   ██╗ ██████╗██╗  ██╗    ██╗     ██╗███████╗██████╗
; ██╔════╝██║   ██║██╔════╝██║ ██╔╝    ██║     ██║██╔════╝██╔══██╗
; █████╗  ██║   ██║██║     █████╔╝     ██║     ██║███████╗██████╔╝
; ██╔══╝  ██║   ██║██║     ██╔═██╗     ██║     ██║╚════██║██╔═══╝
; ██║     ╚██████╔╝╚██████╗██║  ██╗    ███████╗██║███████║██║
; ╚═╝      ╚═════╝  ╚═════╝╚═╝  ╚═╝    ╚══════╝╚═╝╚══════╝╚═╝


;; ARITHMETIC FUNCTIONS
(defun poly+ (p1 p2)
  ; (format t "You called poly+ ~%")
  )

(defun poly- (p1 p2)
  (format t "You called poly- ~%"))

(defun poly* (p1 p2)
  (format t "You called poly* ~%"))

;; Collects similar terms in a given input
(defun poly-simplyfy (input)
  (find-nested input))


;; Loop through the list looking for nested expressions that can be simplyfied
(defun find-nested (input)
  (if (eql input nil)
    (return-from find-nested))
  ;; Check if current position is a nested list
  (if (listp (car input))
    (progn
      (format t "Found nested list: ~d~%" (car input))
      (simplyfy-expression (car input))))
  ;; Loop through the rest of the list and check for nested lists
  (find-nested (rest input)))

;; Checks to see if given input contains an arithmetic symbol
(defun check-for-arithmetic-symbol (input)
  (if (check-if-strings-equal (write-to-string input) '(+ - *))
    T nil))


;; Checks if an input string is equal to a number of other strings
(defun check-if-strings-equal (input vals)
  ;; If vals is empty return false
  (if (eql vals nil) nil)
  ;; If yes return true else recursively call the function
  (if (string-equal input (car vals))
    T  (check-if-strings-equal input (rest vals)))) 


;;; Create interactive prompt to enter data
(defun prompt-read (prompt)
  ;; Code inspired by: gigamonkeys.com/book/practical-a-simple-database.html
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-from-string (read-line *query-io*)))


;;; Prompts the user to enter in a polynomial
(defun prompt-user-input ()
  (return-from prompt-user-input
               (list
                 (prompt-read "Operation [+/-/*]")
                 (prompt-read "Polynomial")
                 (prompt-read "Polynomial"))))

;;; Collects the terms inputted by the user
(defun get-user-input (args)
  ;; Check to see if args have been passed
  (if (> (length args) 0)
      (return-from get-user-input (car args))
    (progn
      (format t "Arguments not passed, would you like to enter some values~%")
      (if (not (y-or-n-p "Choice:"))
        ;; If they choose not to enter input then exit the program
        (progn
          (format t "~%Farewell!~%")
          (ext:exit))
        ;; Else return inputs collected from teh user
        (return-from get-user-input (prompt-user-input))))))


;;; Returns which arithmetic function need to be called
(defun get-arithmetic-function (input)
  ;; Figure out what arithmetic function we need to call
  (if (string-equal (car input) '+)
      (return-from get-arithmetic-function #'poly+))
  (if (string-equal (car input) '-)
    (return-from get-arithmetic-function #'poly-))
  (if (string-equal (car input) '*)
    (return-from get-arithmetic-function #'poly*)))


;;; Main function
(defun main (&rest args)
  ;; Gets the user input and proceses it
  (let ((input (get-user-input args)))
    ;; Simplyfy user input if necessary
    (let ((poly1 (poly-simplyfy (nth 1 input)))
          (poly2 (poly-simplyfy (nth 2 input))))
      (funcall (get-arithmetic-function input) poly1 poly2))))


;; A bunch of simplyfied test cases
; (main '(+ (+ (+ x x) (+ 5y y) (- 2y y)) (+ x y z)))
; (main '(+ (+ (+ 5x x) (- 2x x)) (+ y y)))
; (main '(+ (+ x x) (+ y y)))
; (main)
