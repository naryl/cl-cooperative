
(in-package cl-cooperative.simple)

(defun make-event-loop (size)
  (make-pool size))

(defun run-event-loop (pool)
  (loop while (wakeup pool)
     do (start-pending-jobs pool)))
