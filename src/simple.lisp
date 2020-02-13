
(in-package cl-cooperative.simple)

(defun make-event-loop (size)
  (make-pool size))

(defun run-event-loop (pool)
  (loop :do (start-pending-jobs pool)
        :do (wakeup pool)
        :while (cl-cooperative::threads pool)))
