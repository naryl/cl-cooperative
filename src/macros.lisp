
(in-package :cl-cooperative)

(defmacro run ((&optional pool) &body body)
  "Run a cooperative job in POOL pausing current thread until the job either
finishes or YIELDs"
  (alexandria:with-gensyms (job)
    `(let ((,job (make-instance 'coop :func (lambda () ,@body))))
       (plan-coop ,(or pool '*pool*) ,job)
       ,job)))

(defmacro parallel (() &body body)
  "Execute BODY *in parallel* in the current pool and return its result. The current thread will YIELD and not resume until the job is complete."
  `(progn
     (unless *pool*
       (error "PARALLEL can only be used in a coop thread"))
     (make-parallel (lambda () ,@body))))

(defmacro with-pool ((var size) &body body)
  `(let ((,var (make-pool ,size)))
     (unwind-protect
          (progn
            ,@body)
       (destroy-pool ,var))))
