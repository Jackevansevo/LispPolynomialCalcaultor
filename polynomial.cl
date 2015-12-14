(require "simplyfy.cl")

;; ARITHMETIC FUNCTIONS

;; Adds together two polynomials
(defun poly+ (p1 p2)
  (let ((p1 (simplyfy p1))
        (p2 (simplyfy p2)))
    (let ((result (append '(+)(simplyfy(append '(+) p1 p2)))))
      result)))


;; Subtracts two polynomials
(defun poly- (p1 p2)
  (let ((p1 (simplyfy p1))
        (p2 (simplyfy p2)))
    (let ((result (append '(+)(simplyfy(append '(-) p1 p2)))))
      result)))


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
                 (prompt-read "Operation poly[+/-/*]")
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


;;; Main function
(defun main (&rest args)
  ;; Gets the user input and proceses it
  (let ((input (get-user-input args)))
    ;; Simplyfy user input if necessary
    (funcall (car input) (nth 1 input) (nth 2 input))))
