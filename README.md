[![Build Status](https://travis-ci.com/naryl/cl-cooperative.svg?branch=master)](https://travis-ci.com/naryl/cl-cooperative)

**OS PROCESSES! TONS OF OVERHEAD! DON'T USE UNLESS YOU KNOW WHAT YOU'RE DOING**

At least there are tests now.

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

`MAKE-POOL size &key scheduler` - Creates a cooperative thread pool

`DESTROY-POOL pool` - Cleans up the thread pool

`WITH-POOL (var size) &body body` - Run `body` with an event loop

`WAKEUP pool` - Wakeup one job from the pool

`WAKEUP-ALL pool` - Wakeup all jobs in the pool once

`WAKEUP-UNTIL-RESULT pool job` - `WAKEUP` until job finishes running
and return its result

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

