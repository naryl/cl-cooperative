
(in-package :cl-cooperative-tests)

(def-suite all-tests
    :description "The master suite of all cl-cooperative tests.")

(in-suite all-tests)

(defun test-coop ()
  (run! 'all-tests))

(test dummy-tests
  "Just a placeholder."
  (is (listp (list 1 2)))
  (is (= 5 (+ 2 3))))
