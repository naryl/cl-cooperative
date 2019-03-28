
(defpackage cl-cooperative
  (:use :common-lisp :alexandria)
  (:export #:make-pool

           ;; pool management
           #:wakeup #:wakeup-all #:start-pending-jobs
           #:result

           ;; starting jobs
           #:run #:parallel

           ;; Yielding
           #:yield #:pause #:wait

           ;; Schedulers
           #:round-robin-scheduler))

(defpackage cl-cooperative.simple
  (:nicknames :coop)
  (:use :common-lisp :cl-cooperative)
  (:export #:make-event-loop #:run-event-loop

           ;; starting jobs
           #:run #:parallel

           ;; Yielding
           #:yield #:pause #:wait
           ))
