;; Strips all the integers from a symbol
(defun strip-chars (target input)
  (intern(string-trim target (write-to-string input))))


;; Checks to see if given input contains an arithmetic symbol
(defun is-operator (input)
  (if (member input '(+ - *)) T nil))


(defun get-last-int-pos (input &key (pos 0))
  ;; Find the first position that isn't a number and return the position
  (if (and (numberp (read-from-string input)) (eql pos 0))
    nil
    (if (not(numberp (read-from-string(subseq input 0 1))))
      (return-from get-last-int-pos pos)
      (get-last-int-pos (subseq input 1 (length input)) :pos (+ pos 1)))))


;; Strips all the symbols from the end of a given string
(defun strip-symbols (input)
  (if (and(eql (length input) 1) (has-integers input))
    input
    (let ((start 0)
          (end (get-last-int-pos input)))
      (return-from strip-symbols (subseq input start end)))))


;; Returns true if input contains integers
(defun has-integers (input)
  (if (integerp input)
    T
    (find T (map 'list #'check-if-integer (string input)))))


;; Returns true if character is an integer
(defun check-if-integer (input)
  (if (integerp (digit-char-p input)) T NIl))


;; Returns the integer value in-front of an arithmetic sign
(defun get-sign-value (input)
  (cond
    ((is-operator input) input)
    ((integerp input) input)
    (t (if (has-integers input)
         (parse-integer (strip-symbols (string input))) 1))))


;; Returns true if a list contains intergers
(defun list-has-integers (input)
  (if (some 'integerp input) T Nil))


;; Returns the sign of a given input value
(defun get-sign (input)
  (if (numberp input)
    nil
    (strip-chars "0123456789|" input)))


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
        (concatenate 'string (write-to-string result) (string term))))))


;; Returns a closure that collects terms
(defun make-term-collect (term)
  (lambda (input)
    (if (or (is-operator input) (string-equal term (get-sign input)))
      (get-sign-value input))))


;; Collects integer terms in a given list
(defun collect-integers (input)
  (eval(remove nil(map 'list #'integer-collect? input))))


;; Returns true if x is an integer or an operator
(defun integer-collect?(x)
  (if (or (numberp x) (is-operator x)) x))


;; Flattens a given list
(defun flatten (obj)
  (if (listp obj) (mapcan #'flatten obj) (list obj)))


;; [TODO] Rewrite this using lambdas
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
  (if (has-nested input)
    (flatten(map 'list #'(lambda (x) (simplyfy-term x)) input))
    (simplyfy-term input)))
