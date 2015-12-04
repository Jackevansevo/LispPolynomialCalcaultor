(asdf:defsystem #:polynomial
  :sertial t
  :depends-on (#:cl)
  :components ((:file "polynomial")
               (:file "tests")
               (:file "simplyfy")))

