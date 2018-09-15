
(in-package :cl-cooperative)

#+cooperative-debug
(declaim (speed 0) (debug 3) (safety 3))

(defclass coop-pool ()
  ((threadpool :initarg :threadpool :accessor threadpool)
   (hibernated :accessor hibernated :initform ())
   (threads :accessor threads :initform nil)
   (threads-lock :accessor threads-lock :initform (bt:make-lock "THREADS"))
   (scheduler :accessor scheduler)
   (lock :accessor lock :initform (bt:make-lock "POOL"))
   (cv :accessor cv :initform (bt:make-condition-variable))))

(defvar *thread-cv* nil)
(defvar *pool* nil)

(defun make-pool (size &key (max-size size) (scheduler 'round-robin-scheduler))
  "Create a pool with SIZE os threads"
  (let ((pool (make-instance 'coop-pool
                             :threadpool (cl-threadpool:make-threadpool size :max-queue-size max-size))))
    (setf (scheduler pool) (make-instance scheduler :pool pool))
    (cl-threadpool:start (threadpool pool))
    (bt:acquire-lock (lock pool))
    pool))

(defun yield ()
  "Lets the pool-creating (usually main) thread run"
  (unless *pool*
    (error "YIELD can only be used in a coop thread"))
  (p "Yielding")
  (bt:condition-notify (cv *pool*))
  (bt:condition-wait *thread-cv* (lock *pool*)))

(defun wakeup (pool)
  "Lets a thread determined by the pool's scheduler run"
  (let ((cv (next-thread (scheduler pool))))
    (unless cv
      (p "No thread to wake up")
      (return-from wakeup nil))
    (p "Waking up")
    (bt:condition-notify cv)
    (bt:condition-wait (cv pool) (lock pool))
    t))

(defun wakeup-all (pool)
  "Wakes up all non-hibernated threads once"
  (p "Waking up ~A threads" (length (threads pool)))
  (dolist (cv (threads pool))
    (bt:condition-notify cv)
    (bt:condition-wait (cv pool) (lock pool)))
  t)

;;;; Private

(defun hibernate (pool thread)
  (p "Hibernating")
  (bt:with-lock-held ((threads-lock pool))
    (push thread (hibernated pool))
    (deletef (threads pool) thread)))

(defun unhibernate (pool thread)
  (p "Unhibernating")
  (bt:with-lock-held ((threads-lock pool))
    (push thread (threads pool))
    (deletef (hibernated pool) thread)))

(defun make-concurrent (func)
  (p "Making concurrent job")
  (let ((thread *thread-cv*)
        (pool *pool*)
        (result))
    (hibernate *pool* thread)
    (cl-threadpool:add-job (threadpool *pool*)
                           (lambda ()
                             (setf result (multiple-value-list (funcall func)))
                             (unhibernate pool thread)))
    (yield)
    (apply #'values result)))

(defun make-coop (pool func)
  (p "Making cooperative job")
  (let ((cv (bt:make-condition-variable)))
    (cl-threadpool:add-job (threadpool pool)
                           (lambda ()
                             (let ((*thread-cv* cv)
                                   (*pool* pool))
                               (bt:acquire-lock (lock pool))
                               (unwind-protect
                                    (funcall func)
                                 (bt:with-lock-held ((threads-lock pool))
                                   (deletef (threads pool) cv))
                                 (bt:condition-notify (cv pool))
                                 (bt:release-lock (lock pool))))))
    (bt:with-lock-held ((threads-lock pool))
      (push cv (threads pool))))
  (bt:condition-wait (cv pool) (lock pool)))

;;;; Scheduler(s)

(defgeneric next-thread (scheduler)
  (:documentation "Scheduler is any class which stores the pool and has implementation of this generic. Can return NIL to indicate that no threads can run right now."))

(defclass round-robin-scheduler ()
  ((pool :initarg :pool :accessor pool)
   (thread-num :accessor thread-num :initform 0)))

(defmethod next-thread ((scheduler round-robin-scheduler))
  (with-slots (pool thread-num) scheduler
    (incf thread-num)
    (when (>= thread-num (length (threads pool)))
      (setf thread-num 0))
    (nth thread-num (threads pool))))
