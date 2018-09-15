
(defsystem cl-cooperative
    :depends-on (:cl-threadpool :alexandria :verbose)
    :pathname "src/"
    :serial t
    :components ((:file "defpackage")
                 (:file "pool")
                 (:file "macros")))
