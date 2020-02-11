
**OS PROCESSES! TONS OF OVERHEAD! DON'T USE UNLESS YOU KNOW WHAT YOU'RE DOING**

Also, there are no tests yet.

Ok... So, what is it for?
=========================

This library implements cooperative multithreading using whatever
threads Lisp implementation has. In most of the cases that would be OS
threads. Pretty much the only thing it does is making sure that
exactly one thread is running at any given time, and that no thread is
preempted arbitrarily. The latter may still happen by threads not
managed by cl-cooperative.

Documentation
=============

Pool management
---------------

`MAKE-POOL size &key scheduler` - Creates a cooperative thread pool.

`START-PENDING-JOBS pool` - Start jobs planned with RUN

`WAKEUP pool` - Wakeup one job from the pool

`WAKEUP-ALL pool` - Wakeup all jobs in the pool once

`RESULT job` - Get a job's result

Simplified pool management
--------------------------

Use instead of the full API above. It's basically a loop running
`start-pending-jobs` and `wakeup` until no more jobs left.

`MAKE-EVENT-LOOP size` - Make an event loop with thread pool

`RUN-EVENT-LOOP loop` - Run the loop until no more jobs left. Can be
called on the same event loop more than once.

Schedulers
----------

There's one existing scheduler which is `round-robin-scheduler`. It
simply runs each thread in order. A scheduler is any object that the
following generic specializes on:

`NEXT-THREAD scheduler active-threads` - return the next thread to be
run. `active-threads` is a list of threads that have something to do.

User jobs
---------

`RUN (&optional pool) body` - Plan starting a new job in `pool` (the same pool as
  the current thread by default). It will be started when the main
  thread calls `start-pending-jobs`.

`YIELD` - Yield for a bit to allow other jobs to run

`PAUSE seconds` - Yield for a number of seconds. Can be a fraction.

`WAIT job` - Yield until `job` finishes. Return its result

`PARALLEL () body` - Yield until `body` finishes in parallel, then
  return its result. `body` should only read/write the thread that
  waits on it unless you want to deal with thread synchronization.
