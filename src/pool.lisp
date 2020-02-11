
(in-package :cl-cooperative)

#+cooperative-debug
(declaim (speed 0) (debug 3) (safety 3))

(defclass coop-pool ()
  ((threadpool :initarg :threadpool :accessor threadpool)
   (hibernated :accessor hibernated :initform ())
   (threads :accessor threads :initform nil)
   (threads-lock :accessor threads-lock :initform (bt:make-lock "THREADS"))
   (scheduler :initarg :scheduler :accessor scheduler)
   (lock :accessor lock :initform (bt:make-lock "POOL"))
   (cv :accessor cv :initform (bt:make-condition-variable))
   (pending-jobs :accessor pending-jobs :initform nil)))

(defclass coop ()
  ((cv :accessor cv :initform (bt:make-condition-variable))
   (func :reader func :initarg :func :initform (error "FUNC is mandatory in COOP instances"))
   (result :reader result)
   (sleep-until :accessor sleep-until :initform 0)))

(defvar *coop* nil)
(defvar *pool* nil)

(defun make-pool (size &key (scheduler 'round-robin-scheduler))
  "Create a pool with SIZE os threads"
  (let ((pool (make-instance 'coop-pool
                             :threadpool (cl-threadpool:make-threadpool size :max-queue-size 0)
                             :scheduler scheduler)))
    (cl-threadpool:start (threadpool pool))
    (bt:acquire-lock (lock pool))
    pool))

(defun yield ()
  "Lets the pool-creating (usually main) thread run"
  (unless *pool*
    (error "YIELD can only be used in a coop thread"))
  (p "Yielding")
  (bt:condition-notify (cv *pool*))
  (bt:condition-wait (cv *coop*) (lock *pool*)))

(defun pause (seconds)
  "YIELD for at least designated time"
  (unless *pool*
    (error "PAUSE can only be used in a coop thread"))
  (setf (sleep-until *coop*) (+ (get-universal-time) seconds))
  (yield))

(defun wait (coop)
  "YIELD until a coop thread finishes"
  (unless *pool*
    (error "WAIT can only be used in a coop thread"))
  (loop while (not (slot-boundp coop 'result))
     do (yield))
  (apply #'values (result coop)))

(defun wakeup% (pool coop)
  "Wakes up the thread unless it wants to sleep more"
  (when (< (sleep-until coop)
           (get-universal-time))
    (bt:condition-notify (cv coop))
    (bt:condition-wait (cv pool) (lock pool))))

(defun wakeup (pool)
  "Lets a thread determined by the pool's scheduler run"
  (when *pool*
    (error "WAKEUP can only be used in the main thread"))
  (let ((coop (next-thread (scheduler pool) (active-threads pool))))
    (unless coop
      (p "No thread to wake up")
      (return-from wakeup nil))
    (p "Waking up")
    (wakeup% pool coop)))

(defun active-threads (pool)
  "Returns the list of threads that can run right now"
  (remove-if (lambda (coop)
               (> (sleep-until coop)
                  (get-universal-time)))
             (threads pool)))

(defun wakeup-all (pool)
  "Wakes up all non-hibernated threads once"
  (p "Waking up ~A threads" (length (threads pool)))
  (when *pool*
    (error "WAKEUP-ALL can only be used in the main thread"))
  (unless (threads pool)
    (return-from wakeup-all nil))
  (dolist (c (threads pool))
    (wakeup% pool c)))

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

(defun make-parallel (func)
  (p "Making parallel job")
  (let ((thread *coop*)
        (pool *pool*)
        (result))
    (hibernate pool thread)
    (cl-threadpool:add-job
     (threadpool pool)
     (lambda ()
       (setf result (multiple-value-list (funcall func)))
       (unhibernate pool thread)))
    (yield) ; Will not return until unhibernated
    (apply #'values result)))

(defun start-coop (pool coop)
  (p "Making cooperative job")
  (cl-threadpool:add-job (threadpool pool)
                         (lambda ()
                           (let ((*coop* coop)
                                 (*pool* pool))
                             (bt:acquire-lock (lock pool))
                             (unwind-protect
                                  (setf (slot-value coop 'result) (multiple-value-list (funcall (func coop))))
                               (bt:with-lock-held ((threads-lock pool))
                                 (deletef (threads pool) coop))
                               (bt:condition-notify (cv pool))
                               (bt:release-lock (lock pool))))))
  (bt:with-lock-held ((threads-lock pool))
    (push coop (threads pool)))
  (bt:condition-wait (cv pool) (lock pool)))

(defun plan-job (pool coop)
  (push coop (pending-jobs pool)))

(defun start-pending-jobs (pool)
  (let ((jobs (pending-jobs pool)))
    (setf (pending-jobs pool) nil)
    (dolist (job jobs)
      (start-coop pool job))))

;;;; Scheduler(s)

(defgeneric next-thread (scheduler active-threads)
  (:documentation "Scheduler is any class which implements this generic. Can return NIL to indicate that no threads can run right now.
`active-threads` is the list of threads that can run right now. Consider the objects opaque."))

(defclass round-robin-scheduler ()
  ((current-thread :accessor current-thread :initform nil)))

(defmethod next-thread :around (scheduler threads)
  "If there are no threads to run then don't even ask the scheduler"
  (declare (ignore scheduler))
  (when threads
    (call-next-method)))

(defmethod next-thread ((scheduler round-robin-scheduler) threads)
  (with-slots (current-thread) scheduler
    (let ((next-thread (cdr (member current-thread threads))))
      (setf current-thread (or next-thread (first threads)))
      current-thread)))
