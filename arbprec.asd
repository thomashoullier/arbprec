(defsystem arbprec
  :name "arbprec"
  :author "Thomas HOULLIER"
  :components
  ((:module "src"
    :components ((:file "package")
                 (:file "arbprec" :depends-on ("package"))
                 (:file "words-op" :depends-on ("package"))
                 (:file "conversions" :depends-on ("package"))
                 (:file "compare" :depends-on ("package"))
                 (:file "addition" :depends-on ("package"))
                 (:file "subtraction" :depends-on ("words-op"))
                 (:module "multiplication"
                  :depends-on ("words-op" "addition")
                  :components ((:file "naive")
                               (:file "karatsuba" :depends-on ("naive"))))))))
