;; Returns the integer value in-front of a artihmetic sign
(defun get-sign-value (input)
  (let ((string-input (strip-chars (write-to-string input) "|")))
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
  ;; Check to see if current position is an integer
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
  (if (check-if-strings-equal (write-to-string input) '(+ - *))
    T nil))


;; Checks if an input string is equal to a number of other strings
(defun check-if-strings-equal (input vals)
  ;; If vals is empty return false
  (if (eql vals nil) (return-from check-if-strings-equal nil))
  ;; If yes return true else recursively call the function
  (if (string-equal input (car vals))
    T  (check-if-strings-equal input (rest vals))))


;; Collects terms of a specific target in a given list
(defun collect-terms (target input &key results)
  ;; Check the target is valid
  (if (has-integers (strip-chars(write-to-string target) "|"))
      (return-from collect-terms nil))
  ;; If the list is empty return the results
  (if (eql input nil)
    (let ((result (write-to-string(eval(reverse results))))
          (target (write-to-string target)))
      (return-from collect-terms (concatenate 'string result target))))
  ;; Check if the current list position contains a value equal to our target
  (if (string-equal target (get-sign (car input)))
    ;; Update our results list
    (let ((updated-result (cons (get-sign-value (car input)) results)))
      (collect-terms target (rest input) :results updated-result))
    ;; If not  then check if it's an operator
    (if (has-operator (car input))
      ;; Update our results list
      (let ((updated-result (cons (car input) results)))
        (collect-terms target (rest input) :results updated-result))
      ;; Failing that move onto the next position
      (collect-terms target (rest input) :results results))))


;; Scans through a given list looking for terms to be collected
(defun scan-for-terms (input &key scanned)
  (if (eql input nil)
    (return-from scan-for-terms "finished"))
  (format t "Current pos ~d~%" (car input))
  (if (not (has-operator (car input)))
    (format t "Scanning for: ~d~%" (car input)))
  (scan-for-terms (rest input)))


;; Loop through the list looking for nested expressions that can be simplyfied
(defun find-nested (input)
  (if (eql input nil)
    (return-from find-nested))
  ;; Check if current position is a nested list
  (if (listp (car input))
    (progn
      (format t "Found nested list: ~d~%" (car input))
      (scan-for-terms (car input))))
  ;; Loop through the rest of the list and check for nested lists
  (find-nested (rest input)))
