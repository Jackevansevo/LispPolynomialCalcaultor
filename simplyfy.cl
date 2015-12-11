;; Strips all the integers from a symbol
(defun strip-chars (target input)
  (string-trim target (write-to-string input)))


;; Checks to see if given input contains an arithmetic symbol
(defun is-operator (input)
  (if (member input '(+ - *)) T nil))


;; Returns true if input contains integers
(defun has-integers (input)
  (if (integerp input)
    T
    (find T (map 'list #'check-if-integer (string input)))))


;; Returns true if character is an integer
(defun check-if-integer (input)
  (if (integerp (digit-char-p input)) T NIl))


;; Returns the value of a sign
(defun sign-value (input)
  (if (or (is-operator input) (integerp input)) input
    (if (has-integers input)
      (let ((x (concatenate 'string (write-to-string (sign-of input)) "|")))
        (parse-integer(strip-chars x input))) 1)))


;; Returns true if a list contains intergers
(defun list-has-integers (input)
  (if (some 'integerp input) T Nil))


;; Returns the sign-of of a given input value
(defun sign-of (input)
  (if (numberp input)
    nil
    (intern(strip-chars "0123456789|" input))))


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
      (0 nil)           ;; If result is 0 return nil
      (1 term)          ;; If result is 1 return just the term
      (otherwise
        (intern(concatenate 'string (write-to-string result) (string term)))))))


;; Returns a closure that collects terms
(defun make-term-collect (term)
  (lambda (input)
    (if (or (is-operator input) (string-equal term (sign-of input)))
      (sign-value input))))


;; Collects integer terms in a given list
(defun collect-integers (input)
  (eval(remove nil(map 'list #'integer-collect? input))))


;; Returns true if x is an integer or an operator
(defun integer-collect?(x)
  (if (or (numberp x) (is-operator x)) x))


;; Flattens a given list
(defun flatten (obj)
  (if (listp obj) (mapcan #'flatten obj) (list obj)))


;; [TODO] If I had time I'd rewrite this using lambda expressions
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
      ((not (eql (sign-value (car input)) nil))
       (if (not (is-operator (car input)))
         (if (not(check-if-in-list (sign-of (car input)) scanned))
           (let ((new-results (append results (list(collect-terms (sign-of (car input)) orig))))
                 (new-scanned (append scanned (list(sign-of (car input))))))
             (simplyfy-term (rest input) :scanned new-scanned :orig orig :results new-results))
           (simplyfy-term (rest input) :scanned scanned :orig orig :results results))
         (simplyfy-term (rest input) :scanned scanned :orig orig :results results))))))


;; Returns true if a list contains nested elements
(defun has-nested (input)
  (if (some #'listp input) T nil))


;; Iterates through simplyfying each term in an expression
(defun simplyfy (input)
  (if (has-nested input)
    (simplyfy(flatten(map 'list #'(lambda (x) (simplyfy-term x)) input)))
    (simplyfy-term input)))
