
(defpackage cl-cooperative
  (:use :common-lisp :alexandria)
  (:nicknames :coop)
  (:export #:make-pool
           #:run #:yield-for #:yield #:wakeup #:wakeup-all
           #:round-robin-scheduler))
