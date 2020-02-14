
(in-package :cl-cooperative)

(declaim (optimize (speed 0) (debug 3) (safety 3)))

(defvar *debug-on-error* nil)

(defun ts ()
  "Timestamp in milliseconds"
  (/ (get-internal-real-time)
     internal-time-units-per-second))

(defclass coop-pool ()
  ((threadpool :initarg :threadpool :accessor threadpool)
   (hibernated :accessor hibernated :initform ())
   (threads :accessor threads :initform ())
   (threads-lock :reader threads-lock :initform (bt:make-lock "THREADS"))
   (scheduler :initarg :scheduler :accessor scheduler)
   (pending-jobs :accessor pending-jobs :initform ())
   (lock :reader lock :initform (bt:make-lock "POOL"))
   (cv :reader cv :initform (bt:make-condition-variable))
   (unhibernate-cv :reader unhibernate-cv :initform (bt:make-condition-variable))
   (unhibernate-lock :reader unhibernate-lock :initform (bt:make-lock))))

(defvar *coop-counter* 0)

(defclass coop ()
  ((cv :accessor cv :initform (bt:make-condition-variable))
   (id :reader id :initform (incf *coop-counter*))
   (func :reader func :initarg :func :initform (error "FUNC is mandatory in COOP instances"))
   (result :reader result)
   (waiting-threads :accessor waiting-threads :initform nil)))

(defmethod print-object ((obj coop) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "[~A]" (id obj))
    (when (slot-boundp obj 'result)
      (format stream " Returned: ~S" (result obj)))))

(defvar *coop* nil)
(defvar *pool* nil)
(defvar *in-parallel* nil)

(declaim (inline condition-wait))
(defun condition-wait (cv lock)
  "Was used to detect deadlocks"
  (bt:condition-wait cv lock)
  #+(or)
  (unless (bt:condition-wait cv lock :timeout 10)
    (invoke-debugger (make-condition 'error
                                     "Condition wait timed out.~%Lock is ~S" lock))))

(defun make-pool (size &key (scheduler 'round-robin-scheduler))
  "Create a pool with SIZE os threads"
  (let ((pool (make-instance 'coop-pool
                             :threadpool (cl-threadpool:make-threadpool size :max-queue-size 0)
                             :scheduler (make-instance scheduler))))
    (v:debug :cl-cooperative "Creating a thread pool")
    (cl-threadpool:start (threadpool pool))
    (bt:acquire-lock (lock pool))
    pool))

(defun destroy-pool (pool)
  "Destroys the pool"
  (v:debug :cl-cooperative "Destroying a thread pool")
  (cl-threadpool:stop (threadpool pool)))

(defun yield ()
  "Lets the pool-creating (usually main) thread run"
  (unless *pool*
    (error "YIELD can only be used in a coop thread"))
  (v:debug :cl-cooperative "[~A] Yielding" (id *coop*))
  (bt:condition-notify (cv *pool*))
  (condition-wait (cv *coop*) (lock *pool*)))

(defun pause (seconds)
  "YIELD for at least designated time"
  (unless *pool*
    (error "PAUSE can only be used in a coop thread"))
  (let ((pool *pool*)
        (coop *coop*))
    (let ((unhibernate-timer
            (trivial-timers:make-timer (lambda ()
                                         (unhibernate pool coop))
                                       :name "Coop pause timer")))
      (hibernate pool coop)
      (trivial-timers:schedule-timer unhibernate-timer seconds)
      (yield))))

(defun wakeup-until-result (pool coop)
  "Run the pool until coop produces a result, then return it"
  (loop :while (not (slot-boundp coop 'result))
        :do (wakeup pool))
  (apply #'values (result coop)))

(defun wait (coop)
  "YIELD until a coop thread finishes"
  (unless *pool*
    (error "WAIT can only be used in a coop thread"))
  (push *coop* (waiting-threads coop))
  (hibernate *pool* *coop*)
  (yield)
  (v:debug :cl-cooperative "[~A] Finished waiting for ~A"
           (id *coop*) (id coop))
  (apply #'values (result coop)))

(defun wakeup% (pool coop)
  "Wakes up the thread"
  (bt:condition-notify (cv coop))
  (condition-wait (cv pool) (lock pool)))

(defun wakeup (pool)
  "Lets a thread determined by the pool's scheduler run"
  (when *pool*
    (error "WAKEUP can only be used in the main thread"))
  (start-pending-jobs pool)
  ;; There are no threads running at all
  (when (and (null (threads pool))
             (null (hibernated pool)))
      (v:debug :cl-cooperative "No thread to wake up")
      (return-from wakeup nil))
  ;; Some threads running. Wait until at least one can be woken up
  ;; right now
  (wait-for-active-thread pool)
  (let ((coop (next-thread (scheduler pool) (active-threads pool))))
    (unless coop
      ;; Some thread can be woken up but the scheduler didn't schedule any
      (cerror "Ignore" "No thread to wake up!")
      (return-from wakeup nil))
    (v:debug :cl-cooperative "[~A] Waking up" (id coop))
    (wakeup% pool coop)
    t))

(defun active-threads (pool)
  "Returns the list of threads that can run right now"
  (threads pool))

(defun wait-for-active-thread (pool)
  "Sleeps until a thread becomes active."
  (loop :until (active-threads pool)
        :do (bt:with-lock-held ((unhibernate-lock pool))
              (bt:condition-wait (unhibernate-cv pool)
                                 (unhibernate-lock pool))))
  (values))

(defun wakeup-all (pool)
  "Wakes up all non-hibernated threads once"
  (when *pool*
    (error "WAKEUP-ALL can only be used in the main thread"))
  (v:debug :cl-cooperative "Waking up ~A threads" (length (threads pool)))
  (let ((active-threads (active-threads pool)))
    (unless active-threads
      (return-from wakeup-all nil))
    (dolist (c active-threads)
      (wakeup% pool c))
    t))

(defun hibernate (pool coop)
  (v:debug :cl-cooperative "[~A] Hibernating" (id coop))
  (bt:with-lock-held ((threads-lock pool))
    (push coop (hibernated pool))
    (deletef (threads pool) coop)))

(defun unhibernate (pool coop)
  (v:debug :cl-cooperative "[~A] Unhibernating" (id coop))
  (bt:with-lock-held ((threads-lock pool))
    (push coop (threads pool))
    (deletef (hibernated pool) coop))
  (bt:with-lock-held ((unhibernate-lock pool))
    (bt:condition-notify (unhibernate-cv pool))))

(defun handle-coop-error (e)
  (if *debug-on-error*
      (invoke-debugger e)
      (v:error :cl-cooperative "~S" e)))

(defvar *parallel-counter* 0)
(defun make-parallel (func)
  (let ((thread *coop*)
        (pool *pool*)
        (result)
        (id (incf *parallel-counter*)))
    (when *in-parallel*
      (error "Can't start parallel in another parallel"))
    (hibernate pool thread)
    (cl-threadpool:add-job
     (threadpool pool)
     (lambda ()
       (v:debug :cl-cooperative "[P~A] Starting" id)
       (handler-bind ((error 'handle-coop-error))
         (let ((*in-parallel* t))
           (setf result (multiple-value-list
                         (funcall func))))
         (v:debug :cl-cooperative "[P~A] Finished returning: ~S" id result)
         (unhibernate pool thread))))
    (yield) ; Will not return until unhibernated
    (apply #'values result)))

(defun make-coop-thread (pool coop)
  (lambda ()
    (v:debug :cl-cooperative "[~A] Starting" (id coop))
    (let ((*coop* coop)
          (*pool* pool))
      (bt:with-lock-held ((lock pool))
        (unwind-protect
             (setf (slot-value coop 'result)
                   (multiple-value-list
                    (handler-bind ((error 'handle-coop-error))
                      (funcall (func coop)))))
          (v:debug :cl-cooperative "[~A] Finished returning: ~S"
                   (id coop) (result coop))
          ;; Unhibernating threads that WAIT on this one
          (dolist (th (waiting-threads coop))
            (unhibernate pool th))
          ;; Destroying this thread
          (bt:with-lock-held ((threads-lock pool))
            (deletef (threads pool) coop))
          (bt:condition-notify (cv pool)))))))

(defun plan-coop (pool coop)
  (v:debug :cl-cooperative "[~A] Planning" (id coop))
  (bt:with-lock-held ((threads-lock pool))
    (push coop (pending-jobs pool))))

(defun start-coop (pool coop)
  (cl-threadpool:add-job (threadpool pool)
                         (make-coop-thread pool coop))
  (bt:with-lock-held ((threads-lock pool))
    (push coop (threads pool)))
  (condition-wait (cv pool) (lock pool)))

(defun start-pending-jobs (pool)
  "Start planned coop jobs. If any of them create new jobs before
yielding then start them too."
  (loop :while (pending-jobs pool)
        :do (let ((pending nil))
              (bt:with-lock-held ((threads-lock pool))
                (setf pending (pending-jobs pool))
                (setf (pending-jobs pool) nil))
              (dolist (coop pending)
                (start-coop pool coop)))))

;;;; Scheduler(s)

(defgeneric next-thread (scheduler active-threads)
  (:documentation "Scheduler is any class which implements this generic. Can return NIL to indicate that no threads can run right now.
`active-threads` is the list of threads that can run right now. Consider the objects opaque.")
  (:method (scheduler threads)
    (cerror "Ignore " "Unknown scheduler: ~S" scheduler)
    (car threads)))

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
