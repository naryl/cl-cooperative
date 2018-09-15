
(in-package :cl-cooperative)

(defmacro run ((pool) &body body)
  "Run a cooperative job in POOL pausing current thread until the job either
finishes or YIELDs"
  `(make-coop ,pool (lambda () ,@body)))

(defmacro yield-for (() &body body)
  "Execute BODY *concurrently* in the current pool and return its result. The current thread will YIELD and not resume until the job is completed."
  `(progn
     (unless *pool*
       (error "YIELD-FOR can only be used in a coop thread"))
     (make-concurrent (lambda () ,@body))))
