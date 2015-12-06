;; Returns the integer value in-front of a artihmetic sign
(defun get-sign-value (input)
  (let ((string-input (strip-chars (write-to-string input) "|")))
    ;; If it's an operator then just return it's value
    (if (has-operator input)
      (return-from get-sign-value input))
    ;; If the string doesn't contain any integers then just return 1
    (if (not (has-integers string-input))
      1
      (parse-integer (strip-symbols string-input)))))


;; Strip characters from a string
;; http://rosettacode.org/wiki/Strip_a_set_of_characters_from_a_string#Common_Lisp
(defun strip-chars (str chars)
  (remove-if (lambda (ch) (find ch chars)) str))


;; Loops through a string and looks for integers
(defun has-integers (input)
  ;; If the string is empty then quit
  (if (eql (length input) 0)
      (return-from has-integers Nil))
  (if (numberp (read-from-string(subseq input 0 1)))
    (return-from has-integers T)
    ;; Else check the next character in the string
    (has-integers (subseq input 1 (length input)))))


;; Strips all the symbols from the end of a given string
(defun strip-symbols (input)
  (let ((start 0)
        (end (get-last-integer-position input)))
    (return-from strip-symbols (subseq input start end))))


;; Strips all the integers from the start of a given string
(defun strip-integers (input)
  (let ((start (get-last-integer-position input))
        (end (length input)))
    (return-from strip-integers (subseq input start end))))


;; Returns the position of the last integer within a list
(defun get-last-integer-position (input &key (pos 0))
  ;; Find the first position that isn't a number and return the position
  (if (not(numberp (read-from-string(subseq input 0 1))))
    (return-from get-last-integer-position pos))
    (get-last-integer-position (subseq input 1 (length input)) :pos (+ pos 1)))


;; Returns the sign of a given input value
(defun get-sign (input)
  (if (eql (length (write-to-string input))1)
    (return-from get-sign (write-to-string input))
    ;; Convert the input to a string and strip off annoying '|' characters
    (strip-integers (strip-chars (write-to-string input) "|"))))


;; Checks to see if given input contains an arithmetic symbol
(defun has-operator (input)
  (if (check-if-strings-equal (write-to-string input) '(+ - * expt))
    T nil))


;; Checks if an input string is equal to a number of other strings
(defun check-if-strings-equal (input vals)
  ;; If vals is empty return false
  (if (eql vals nil) (return-from check-if-strings-equal nil))
  ;; If yes return true else recursively call the function
  (if (string-equal input (car vals))
    T  (check-if-strings-equal input (rest vals))))


;; Collects terms of a specific target in a given list
(defun collect-terms (target input &key (results '()))
  (if (eql input nil)
    (let ((result (write-to-string(eval(reverse results)))))
      (return-from collect-terms (concatenate 'string result target))))
  ;; Check if the current position is an operator
  (if (or (has-operator (car input)) (string-equal target (get-sign (car input))))
    (collect-terms target (rest input) :results (cons (get-sign-value(car input)) results))
    (collect-terms target (rest input) :results results)))


;; Collects integer terms in a given list
(defun collect-integers (input &key (results '()))
  (if (eql input nil)
    (let ((result (write-to-string(eval(reverse results)))))
      (return-from collect-integers (concatenate 'string result))))
  (if (or (numberp (car input)) (has-operator (car input)))
    (collect-integers (rest input) :results (cons (car input) results))
    (collect-integers (rest input) :results results)))


;; Checks if a given item is in a list
(defun check-if-in-list (target input)
  ;; If the input is empty then return false
  (if (eql input nil)
    nil
    (if (equal (car input) target)
      T
      (check-if-in-list target (rest input)))))

;; Check if a list contains integers
(defun check-list-for-integers (input)
  (if (eql input nil)
    (return-from check-list-for-integers nil))
  (if (integerp (read-from-string(car input)))
    (return-from check-list-for-integers T))
  (check-list-for-integers (rest input)))


;; Scansn through a given list looking for terms to be collected
(defun collect-symbols (input &key (orig input) (scanned '()) (results '()))
  ;; If the list is empty then return from the function
  (if (eql input nil)
    (return-from collect-symbols results))
  ;; Set a variable to track the current position in the list
  (let ((target (get-sign (car input))))
    ;; We don't want to collect operators
    (if (has-operator (car input))
      (collect-symbols (rest input) :scanned scanned :orig orig :results results)
      ;; Check if the symbol of the current position has been scanned
      (if (check-if-in-list target scanned)
        (collect-symbols (rest input) :scanned scanned :orig orig :results results)
        (let ((new-scanned (append scanned (list target)))
              (new-results (append results (list(collect-terms target orig)))))
          (collect-symbols (rest input) 
                           :orig orig 
                           :scanned new-scanned 
                           :results new-results))))))

;; Loop through the list looking for nested expressions that can be simplyfied
(defun find-nested (input &key (orig input))
  (if (eql input nil)
    (progn
      (return-from find-nested (collect-symbols orig))))
  ;; Check if current position is a nested list
  (if (listp (car input))
    (progn
      (format t "~%Simplyfying: ~d~%" (car input))
      (collect-symbols (car input))))
  ;; Loop through the rest of the list and check for nested lists
  (find-nested (rest input) :orig orig))


(format t "~d~%" (collect-integers '(+ 1 2 3 4x 4y 5zy 100)))
(format t "~d~%" (collect-symbols '(+ x 5x 7x 4y y y z z z)))
