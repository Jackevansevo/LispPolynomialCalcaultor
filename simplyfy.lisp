;; Returns the integer value in-front of a artihmetic sign
(defun get-sign-value (input)
  (let ((string-input (strip-chars (write-to-string input) "|")))
    ;; If the character is just a single x, y or z then return 1
    (if (eql (length string-input) 1)
      1
      (parse-integer (remove-last-character string-input)))))

;; Removes the last character from a given string
(defun remove-last-character (input)
  (subseq input 0 (1- (length input))))

;; Returns the sign of a given input value
(defun get-sign (input)
  ;; Convert the input to a string and strip off annoying '|' characters
  (get-last-character (strip-chars (write-to-string input) "|")))

;; Returns the last character of a tring
(defun get-last-character (input)
  (subseq input (- (length input) 1) (length input)))

;; Strip characters from a string
;; http://rosettacode.org/wiki/Strip_a_set_of_characters_from_a_string#Common_Lisp
(defun strip-chars (str chars)
  (remove-if (lambda (ch) (find ch chars)) str))

;; Checks to see if given input contains an arithmetic symbol
(defun has-operator (input)
  (if (check-if-strings-equal (write-to-string input) '(+ - *))
    T Nil))

;; Checks if an input string is equal to a number of other strings
(defun check-if-strings-equal (input vals)
  ;; If vals is empty return false
  (if (eql vals nil) (return-from check-if-strings-equal Nil))
  ;; If yes return true else recursively call the function
  (if (string-equal input (car vals))
    T  (check-if-strings-equal input (rest vals))))

;; Collects terms of a specific target in a given list
(defun collect-terms (target input &key results)
  ;; If the list is empty return the results
  (if (eql input nil)
      (return-from collect-terms (eval(reverse results))))
  ;; Check if the current list position contains a value equal to our target
  (if (string-equal target (get-sign (car input)))
    (collect-terms target (rest input) :results (cons (get-sign-value (car input)) results))
    ;; If not  then check if it's an operator
    (if (has-operator (car input))
      (collect-terms target (rest input) :results (cons (car input) results))
      ;; Failing that move onto the next position
      (collect-terms target (rest input) :results results))))


(format t "Collected terms: ~d~%" (collect-terms 'x '(+ 5x 2x)))
