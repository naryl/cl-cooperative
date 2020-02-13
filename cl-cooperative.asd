
(defsystem :cl-cooperative
  :depends-on (:cl-threadpool :alexandria :verbose)
  :pathname "src/"
  :serial t
  :components ((:file "defpackage")
               (:file "dev")
               (:file "pool")
               (:file "macros")
               )

  :in-order-to ((test-op (test-op "cl-cooperative/tests")))
  :perform (test-op (o s)
             (ql:quickload 'fiveam)
             (uiop:symbol-call :fiveam :run!
                               (intern "ALL-TESTS" :cl-cooperative-tests))))

(defsystem #:cl-cooperative/tests
  :depends-on (:cl-cooperative :fiveam)
  :components ((:module "t"
                        :serial t
                        :components ((:file "package")
                                     (:file "main")))))
