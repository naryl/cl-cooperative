
(in-package cl-cooperative.simple)

(defun make-loop (size)
  (make-pool size))

(defun run-loop (pool)
  (loop while (wakeup pool)
     do (start-pending-jobs pool)))
