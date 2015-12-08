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


(defun get-last-integer-position (input &key (pos 0))
  ;; Find the first position that isn't a number and return the position
  (if (and (numberp (read-from-string input) ) (eql pos 0))
    nil
    (if (not(numberp (read-from-string(subseq input 0 1))))
      (return-from get-last-integer-position pos)
      (get-last-integer-position (subseq input 1 (length input)) :pos (+ pos 1)))))


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
          (end (get-last-integer-position input)))
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


;; Collects terms of a specific target in a given list
(defun collect-terms (target input &key (results '()))
  (if (string-has-integers target)
    (return-from collect-terms nil))
  (if (eql input nil)
    (let ((result (write-to-string(eval(reverse results)))))
      (if (string-equal result "1") (return-from collect-terms target)
        (return-from collect-terms (concatenate 'string result target)))))
  (let ((currentPos (car input)))
    ;; Check if the current position is an operator
    (if (or (is-operator currentPos) (string-equal target (get-sign currentPos)))
      (collect-terms target (rest input) :results (cons (get-sign-value currentPos) results))
      (collect-terms target (rest input) :results results))))


;; Collects integer terms in a given list
(defun collect-integers (input &key (results '()))
  (if (eql input nil)
    (let ((result (write-to-string(eval(reverse results)))))
      (return-from collect-integers (concatenate 'string result))))
  (if (or (numberp (car input)) (is-operator (car input)))
    (collect-integers (rest input) :results (cons (car input) results))
    (collect-integers (rest input) :results results)))


; ;; Scans through a given list looking for terms to be collected
; (defun simplyfy (input &key (orig input) (scanned '()) (results '()))
;   ;; If the list is empty then return from the function
;   (if (eql input nil)
;     (return-from simplyfy results))
;   ;; Set a variable to track the current position in the list
;   (let ((target (get-sign (car input))))
;     ;; We don't want to collect operators
;     (if (is-operator (car input))
;       (simplyfy (rest input) :scanned scanned :orig orig :results results)
;       ;; Check if the symbol of the current position has been scanned
;       (if (check-if-in-list target scanned)
;         (simplyfy (rest input) :scanned scanned :orig orig :results results)
;         (let ((new-scanned (append scanned (list target)))
;               (new-results (append results (list(collect-terms target orig)))))
;           (simplyfy (rest input) 
;                            :orig orig 
;                            :scanned new-scanned 
;                            :results new-results))))))


(defun simplyfy (input &key (orig input) (scanned '()) (results '()))
  ;; If the list is empty then return the results set
  (if (eql input nil)
    (return-from simplyfy results))
  ;; Set a variable to track the current position in the list
  (format t "~%Curent position: ~d~%" (car input))
  (format t "Scanned: ~d~%" scanned)
  (cond
    ;; If it's an integer
    ((integerp (car input))
     (format t "Found integer~%")
     (if (not(list-has-integers scanned))
       (progn
         (format t "Integer not in scanned~%")
         (let ((new-results (append results (list(collect-integers orig))))
               (new-scanned (append scanned (list(car input)))))
           (simplyfy (rest input) :scanned new-scanned :orig orig :results new-results)))
       (progn
         (format t "Integers already scanned~%")
         (simplyfy (rest input) :scanned scanned :orig orig :results results))))
    ;; If it's a number with a symbol
    ((not (eql (get-sign-value (car input)) nil))
     (if (not (is-operator (car input)))
       (if (not(check-if-in-list (get-sign (car input)) scanned))
         (progn
           (format t "Symbol ~d hasn't been scanned~%" (get-sign (car input)))
           (let ((new-results (append results (list(collect-terms (get-sign (car input)) orig))))
                 (new-scanned (append scanned (list(get-sign (car input))))))
             (simplyfy (rest input) :scanned new-scanned :orig orig :results new-results)))
         (progn
           (format t "Symbol ~d has already been scanned~%" (get-sign (car input)))
           (simplyfy (rest input) :scanned scanned :orig orig :results results)))
       (progn
         (format t "Skipping operator~%")
         (simplyfy (rest input) :scanned scanned :orig orig :results results))))))


;; Loop through the list looking for nested expressions that can be simplyfied
(defun find-nested (input &key (orig input))
  (if (eql input nil)
    (progn
      (return-from find-nested nil)))
  ;; Check if current position is a nested list
  (if (listp (car input))
    (progn
      (format t "~%Found nested ~d~%" (car input))
      (format t "Collected symbols: ~d~%" (simplyfy (car input)))
      (if (list-has-integers (car input))
        (format t "Collected integers ~d~%" (collect-integers (car input))))))
  ;; Loop through the rest of the list and check for nested lists
  (find-nested (rest input) :orig orig))
