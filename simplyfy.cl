;; Strips all the integers from the start of a given string
(defun strip-chars (target input)
  (string-trim target input))


;; Returns true if input string can be found in list of target strings
(defun check-if-strings-equal (input targets)
  ;; If targets is empty return false
  (if (eql targets nil)
    nil
    ;; Check if string is equal to current position in target
    (if (string-equal input (car targets))
      T  (check-if-strings-equal input (rest targets)))))


; Checks to see if given input contains an arithmetic symbol
(defun is-operator (input)
  (if (check-if-strings-equal (write-to-string input) '(+ - * expt))
    T nil))


(defun get-last-int-pos (input &key (pos 0))
  ;; Find the first position that isn't a number and return the position
  (if (and (numberp (read-from-string input) ) (eql pos 0))
    nil
    (if (not(numberp (read-from-string(subseq input 0 1))))
      (return-from get-last-int-pos pos)
      (get-last-int-pos (subseq input 1 (length input)) :pos (+ pos 1)))))


;; Returns true if a string contains integers
(defun string-has-integers (input)
  ;; If the string is empty then quit
  (if (string-equal input "")
    nil
    (if (numberp (read-from-string(subseq input 0 1)))
      (return-from string-has-integers T)
      ;; Else check the next character in the string
      (string-has-integers (subseq input 1 (length input))))))


;; Strips all the symbols from the end of a given string
(defun strip-symbols (input)
  (if (and(eql (length input) 1) (string-has-integers input))
    input
    (let ((start 0)
          (end (get-last-int-pos input)))
      (return-from strip-symbols (subseq input start end)))))


;; Returns the integer value in-front of a artihmetic sign
(defun get-sign-value (input)
  (let ((string-input (strip-chars "|" (write-to-string input))))
    ;; If it's an operator then just return it's value
    (if (is-operator input) input
      ;; If there's intergers infront e.g. '50xy' strip them and return the
      ;; value, else just return 1, e.g. for 'y'
      (if (string-has-integers string-input)
        (parse-integer (strip-symbols string-input)) 1))))


;; Returns true if a list contains intergers
(defun list-has-integers (input)
  (if (some 'integerp input) T Nil))


;; Returns the sign of a given input value
(defun get-sign (input)
  (if (numberp input)
    nil
    (strip-chars "0123456789|" (write-to-string input))))


;; Returns true if target can be found in list
(defun check-if-in-list (target input)
  ;; If the input is empty then return false
  (if (eql input nil)
    nil
    (if (equal (car input) target)
      T
      (check-if-in-list target (rest input)))))


;; Collects terms within a given list
(defun collect-terms (term input)
  (let ((result (eval (remove nil(map 'list (make-term-collect term) input)))))
    (case result
      (0 nil)               ;; If result is 0 return nil
      (1 term)              ;; If result is 1 return just the term
      (otherwise
        (concatenate 'string (write-to-string result) term)))))


;; Returns a closure that collects terms
(defun make-term-collect (term)
  (lambda (input)
    (if (or (is-operator input) (string-equal term (get-sign input)))
      (get-sign-value input))))


;; Evaluates whether terms should be collected or not
(defun term-collect? (term input)
  (format t "Input ~d~%" input)
  (format t "Term ~d~%" input))


;; Collects integer terms in a given list
(defun collect-integers (input)
  (eval(remove nil(map 'list #'integer-collect? input))))


;; Returns true if x is an integer or an operator
(defun integer-collect?(x)
  (if (or (numberp x) (is-operator x)) x))

;; Flattens a given list
(defun flatten (obj)
  (if (listp obj) (mapcan #'flatten obj) (list obj)))


;; Do I date try to rewrite this using maps and lambdas lol

;; Returns a simplyfied version of the polynomial expression
(defun simplyfy-term (input &key (orig input) (scanned '()) (results '()))
  (if (eql input nil)
    results
    (cond
      ;; If it's not a list
      ((not (listp input))
       (return-from simplyfy-term input))
      ;; If it's an integer
      ((integerp (car input))
       (if (not(list-has-integers scanned))
         (let ((new-results (append results (list(collect-integers orig))))
               (new-scanned (append scanned (list(car input)))))
           (simplyfy-term (rest input) :scanned new-scanned :orig orig :results new-results))
         (simplyfy-term (rest input) :scanned scanned :orig orig :results results)))
      ;; If it's a number with a symbol
      ((not (eql (get-sign-value (car input)) nil))
       (if (not (is-operator (car input)))
         (if (not(check-if-in-list (get-sign (car input)) scanned))
           (let ((new-results (append results (list(collect-terms (get-sign (car input)) orig))))
                 (new-scanned (append scanned (list(get-sign (car input))))))
             (simplyfy-term (rest input) :scanned new-scanned :orig orig :results new-results))
           (simplyfy-term (rest input) :scanned scanned :orig orig :results results))
         (simplyfy-term (rest input) :scanned scanned :orig orig :results results))))))


;; Returns true if a list contains nested elements
(defun has-nested (input)
  (if (some #'listp input) T nil))


;; Iterates through simplyfying each term in an expression
(defun simplyfy (input)
  (format t "Passed input: ~d~%" input)
  (if (has-nested input)
    (flatten(map 'list #'(lambda (x) (simplyfy-term x)) input))
    (simplyfy-term input)))

(format t "~d~%" (collect-terms "2x" '(+ x x x y z)))
