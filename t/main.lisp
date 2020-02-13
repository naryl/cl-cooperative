
(in-package :cl-cooperative-tests)

(def-suite all-tests
  :description "The master suite of all cl-cooperative tests.")

(def-suite full-tests
  :description "Tests for the full api, and some thread functions"
  :in all-tests)

(def-suite simple-tests
  :description "Tests for the simple api, and most thread functions"
  :in all-tests)

(in-suite full-tests)

(test test-pool
  (let ((pool (cl-cooperative:make-pool 2)))
    (let ((gt1 (cl-cooperative:run (pool) 42))
          (gt2 (cl-cooperative:run (pool) 43)))
      (cl-cooperative:start-pending-jobs pool)
      (is (cl-cooperative:wakeup-until-result pool gt1) 42)
      (is (cl-cooperative:wakeup-until-result pool gt2) 43))))

(test test-wait
  (let ((pool (cl-cooperative:make-pool 2)))
    (let* ((gt1 (cl-cooperative:run (pool) 42))
           (gt2 (cl-cooperative:run (pool)
                  (cl-cooperative:wait gt1)
                  (cl-cooperative:result gt1))))
      (cl-cooperative:start-pending-jobs pool)
      (is (cl-cooperative:wakeup-until-result pool gt2) 42))))

(test test-wakeup
  (let ((pool (cl-cooperative:make-pool 2)))
    (let ((gt1 (cl-cooperative:run (pool) 42))
          (gt2 (cl-cooperative:run (pool) 43)))
      (cl-cooperative:start-pending-jobs pool)
      (is (cl-cooperative:wakeup-until-result pool gt1) 42)
      (is (cl-cooperative:wakeup-until-result pool gt2) 43))))

(in-suite simple-tests)

(test test-loop
  (let* ((loop (coop:make-event-loop 2))
         (gt1 (coop:run (loop) 42)))
    (coop:run-event-loop loop)
    (is (cl-cooperative:wakeup-until-result loop gt1) 42)))

(test test-parallel
  (let ((loop (coop:make-event-loop 2)))
    (let ((gt1 (coop:run (loop)
                 (coop:parallel ()
                   42))))
      (coop:run-event-loop loop)
      (is (cl-cooperative:wakeup-until-result loop gt1) 42))

    ;; Yep, this makes no practical sense
    #+nil
    (let ((gt1 (coop:run (loop)
                 (coop:wait
                  (coop:run ()
                    (coop:parallel ()
                      42))))))
      (coop:run-event-loop loop)
      (is (cl-cooperative:wakeup-until-result loop gt1) 42))))

(test test-sleep
  (let* ((loop (coop:make-event-loop 2))
         (gt1 (coop:run (loop)
                (coop:pause 1)
                42)))
    (coop:run-event-loop loop)
    (is (cl-cooperative:wakeup-until-result loop gt1) 42)))


