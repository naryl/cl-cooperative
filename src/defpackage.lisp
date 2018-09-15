
(defpackage cl-cooperative
  (:use :common-lisp :alexandria)
  (:nicknames :coop)
  (:export #:make-pool
           #:run #:yield-for #:yield #:wakeup
           #:round-robin-scheduler))
