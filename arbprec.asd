(defsystem arbprec
  :name "arbprec"
  :author "Thomas HOULLIER"
  :components
  ((:module "src"
    :components ((:file "package")
                 (:file "arbprec" :depends-on ("package"))
                 (:file "words-op" :depends-on ("package" "arbprec"))
                 (:file "conversions" :depends-on ("package"))
                 (:file "compare" :depends-on ("package"))
                 (:file "addition" :depends-on ("package"))
                 (:file "subtraction" :depends-on ("words-op"))
                 (:module "multiplication"
                  :depends-on ("words-op" "addition")
                  :components ((:file "naive")
                               (:file "karatsuba" :depends-on ("naive"))))
                 (:file "operations"
                  :depends-on ("addition" "subtraction" "multiplication")))))
  :in-order-to ((test-op (test-op "arbprec/test"))))

(defsystem arbprec/test
  :name "arbprec/test"
  :author "Thomas HOULLIER"
  :depends-on ("rove" "arbprec")
  :components
  ((:module "test"
    :components ((:file "rove-suite"))))
  :perform (test-op (o c) (symbol-call :rove '#:run-suite :arbprec/test)))
