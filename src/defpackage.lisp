
(defpackage cl-cooperative
  (:use :common-lisp :alexandria)
  (:nicknames :coop)
  (:export #:make-pool
           #:destroy-pool
           #:with-pool

           ;; pool management
           #:wakeup
           #:wakeup-all
           #:wakeup-until-result

           ;; starting jobs
           #:run
           #:parallel

           ;; Yielding
           #:yield
           #:pause
           #:wait

           ;; Schedulers
           #:round-robin-scheduler
           #:next-thread
           ))
