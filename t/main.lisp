
(in-package :cl-cooperative-tests)

(setf (v:repl-level) :debug)
(setf (v:repl-categories) '(:cl-cooperative))

(def-suite all-tests
  :description "The master suite of all cl-cooperative tests.")

(def-suite full-tests
  :description "All the tests are here for now"
  :in all-tests)

(in-suite full-tests)

(test test-pool
  (coop:with-pool (pool 2)
    (let ((gt1 (coop:run (pool) 42))
          (gt2 (coop:run (pool) 43)))
      (is (coop:wakeup-until-result pool gt1) 42)
      (is (coop:wakeup-until-result pool gt2) 43))))

(test test-wait
  (coop:with-pool (pool 2)
    (let* ((gt1 (coop:run (pool) 42))
           (gt2 (coop:run (pool)
                  (coop:wait gt1))))
      (is (coop:wakeup-until-result pool gt2) 42))))

(test test-sleep
  (coop:with-pool (pool 2)
    (let ((gt1 (coop:run (pool)
                 (coop:pause 1)
                 42)))
      (is (coop:wakeup-until-result pool gt1) 42))))

(test test-parallel
  (coop:with-pool (pool 2)
    (let ((gt1 (coop:run (pool)
                 (coop:parallel ()
                   42))))
      (is (coop:wakeup-until-result pool gt1) 42))))

(test test-nested-run
  (coop:with-pool (pool 3)
    (let ((gt1 (coop:run (pool)
                 (coop:wait
                  (coop:run ()
                    42)))))
      (is (coop:wakeup-until-result pool gt1) 42))))

(test test-nested-run-parallel
  (coop:with-pool (pool 3)
    (let ((gt1 (coop:run (pool)
                 (coop:wait
                  (coop:run ()
                    (coop:parallel ()
                      42))))))
      (is (coop:wakeup-until-result pool gt1) 42))))

(test test-nested-parallel
  (coop:with-pool (pool 3)
    (let ((gt1 (coop:run (pool)
                 (coop:parallel ()
                   (handler-case
                       ;; This should throw
                       (coop:parallel ()
                         41)
                     (error () 42))))))
      (is (coop:wakeup-until-result pool gt1) 42))))
