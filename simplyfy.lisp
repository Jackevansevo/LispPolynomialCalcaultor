; ;; Checks to see if given input contains an arithmetic symbol
; (defun check-for-arithmetic-symbol (input)
;   (if (check-if-strings-equal (write-to-string input) '(+ - *))
;     T Nil))


; ;; Checks if an input string is equal to a number of other strings
; (defun check-if-strings-equal (input vals)
;   ;; If vals is empty return false
;   (if (eql vals nil) (return-from check-if-strings-equal Nil))

;   ;; If yes return true else recursively call the function
;   (if (string-equal input (car vals))
;     T  (check-if-strings-equal input (rest vals))))

; ;; Simplyfies a polynomial (Hopefully)
; (defun simplyfy (input &key (counter 1 sign-supplied-p) (sign Nil sign-supplied-p))
;   ;; If the list is empty, we've finished processing
;   (if (eql input nil)
;     (return-from simplyfy))
;   ;; If it's a symbol
;   (if (check-for-arithmetic-symbol (car input))
;     (simplyfy (rest input) :sign (car input)))
;     ;; It it's a number
;     (progn
;       ;; Check to see how many times this number occurs in the polynomial
;       (get-number-of-occurances (car input) input)

;       ;; If it's the same then increment the counter by one
;       (simplyfy (rest input) :counter (+ counter 1) :sign sign )
;       ;; Else then don't increment the counter
;       (simplyfy (rest input) :counter counter :sign sign )))



;; [TODO] Make this work for 5x and 8y
;; Returns the number of occurances of a number/symbol in a polynomial
(defun get-number-of-occurances (target input &key (counter 0))
  (if (eql input nil)
    (return-from get-number-of-occurances counter))

  ;; Check if current position in list matches the target
  (if (eql target (car input))
    ;; If it does then increment the counter
    (get-number-of-occurances target (rest input) :counter (+ counter 1))
    ;; Else keep scanning the list
    (get-number-of-occurances target (rest input) :counter counter)))


(format t "Count: ~d~%" (get-number-of-occurances 'x '(+ x 5x)))
