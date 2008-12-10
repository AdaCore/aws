"""Generic loop for testsuites

This package provides a class called MainLoop that provides a generic
implementation of a testsuite main loop. Parallelism, abortion and time
control are the key features.

Each MainLoop instance controls a set of Workers whose number is set
by the user. The list of tasks/tests to be achieved by the workers,
is provided by a list. The mainloop distribute the elements to the
the workers when they have nothing to do. Usually, an element is a
string identifying the test to be run. An element can also be a list
in that case the worker will execute sequentially each "subelement".
This case is used to adress dependency between tests (occurs for
example with the ACATS).

When a worker is asked to run a test, the command is executed by
calling run_testcase (testid). Once a test is finished the function
collect_result will be called with test id, and process (a
gnatpython.ex.Run object) and the job_info as parameters. Both
run_testcase and collect_result are user defined functions.

Note also that from the user point view there is no parallelism to handle.
The two user defined function run_testcase and collect_result are called
sequentially.
"""

import logging
import os
from time import sleep

logger = logging.getLogger ('gnatpython.mainloop')

class NeedRequeue (Exception):
    """Raised by collect_result if a test need to be requeued"""
    pass

class Worker (object):
    """Run run_testcase and collect_result"""
    def __init__ (self, items, run_testcase, collect_result, slot):
        """Worker constructor

        PARAMETERS
          items: item or list of items to be run by the worker
          run_testcase: command builder function (see MainLoop doc)
          collect_result: result processing function (see MailLoop doc)

        RETURN VALUE
          a Worker instance

        REMARKS
          None
        """
        self.run_testcase   = run_testcase
        self.collect_result = collect_result
        self.slot           = slot

        # Count the number of retry for the current test
        self.nb_retry = 0

        if isinstance (items, list):
            self.jobs = items.reverse ()
        else:
            self.jobs = [items]

        self.current_process = None
        self.current_job     = None
        self.execute_next ()

    def execute_next (self):
        """Execute next worker item/test

        PARAMETERS
          None

        RETURN VALUE
          return False if the worker has nothing to do. True if a test is
          launched.

        REMARKS
          None
        """
        if len (self.jobs) == 0:
            return False
        else:
            self.current_job = self.jobs.pop()

            job_info = (self.slot, self.nb_retry)
            self.current_process = self.run_testcase (self.current_job,
                                                      job_info)
            return True

    def poll (self):
        """Test if a test/item is still executing

        PARAMETERS
          None

        RETURN VALUE
          True if busy, False otherwise.

        REMARKS
          None
        """
        result = self.current_process.poll ()
        if result is not None:
            # Current process has finished
            self.wait ()
            return False
        else:
            return True

    def wait (self):
        """Wait for a test/item to finish

        PARAMETERS
          None

        RETURN VALUE
          None

        REMARKS
          The collect_result function is called upon test/item termination
        """
        self.current_process.wait ()

        try:
            job_info = (self.slot, self.nb_retry)
            self.collect_result (self.current_job,
                                 self.current_process,
                                 job_info)
            self.current_job     = None
            self.current_process = None

        except NeedRequeue:
            # Reinsert the current job in the job list
            self.nb_retry += 1
            self.jobs.append(self.current_job)

class MainLoop (object):
    """Run a list of jobs"""
    def __init__ (self,
                  item_list,
                  run_testcase,
                  collect_result,
                  parallelism=1,
                  abort_file=None,
                  dyn_poll_interval=True):
        """Launch loop

        PARAMETERS
          item_list: a list of jobs

          run_testcase: a function that takes a job for argument
                        and return the spawned process (ex.Run object). Its
                        prototype should be func (name, job_info).
                        name: job identifier
                        job_info: related information, passed in a tuple:
                        (slot_number, job_retry)
                          slot_number: identifier of the Worker that is used
                          to run this testcase.
                          job_retry: number of times that the testcase have
                          been run already.

                        Note that if you want to take advantage of the
                        parallelism the spawned process should be launched
                        in background (ie with bg=True when using ex.Run)

          collect_result: a function called when a job is
                          finished. The prototype should be func
                          (name, process, job_info). If collect_result
                          raise NeedRequeue then the test will be
                          requeued.
                          job_info is a tuple: (slot_number, job_nb_retry)

          parallelism: number of workers

          abort_file: If specified, the loop will abort if the file is present

          dyn_poll_interval: If True the interval between each polling
                     iteration is automatically updated. Otherwise it's set to
                     0.1 seconds

        RETURN VALUE
          a MainLoop instance

        REMARKS
          None
        """
        self.abort_file    = abort_file
        self.workers       = [None] * parallelism
        iterator           = item_list.__iter__ ()
        active_workers     = 0
        max_active_workers = parallelism
        poll_sleep         = 0.1

        try:
            while True:
                # Check for abortion
                if abort_file is not None and os.path.isfile (abort_file):
                    logger.info ('Aborting: file %s has been found'
                                 % abort_file)
                    self.abort()
                    return      # Exit the loop

                # Find free workers
                for slot, worker in enumerate (self.workers):
                    if worker is None:
                        # a worker slot is free so use it for next job
                        logger.debug ('Active worker on slot %d' % slot)
                        next_job = iterator.next ()
                        self.workers[slot] = Worker (next_job,
                                                     run_testcase,
                                                     collect_result,
                                                     slot)
                        active_workers += 1

                poll_counter = 0
                logger.debug ('Wait for free worker')
                while active_workers >= max_active_workers:
                    # All worker are occupied so wait for one to finish
                    poll_counter += 1
                    for slot, worker in enumerate (self.workers):
                        # Test if the worker is still active and have more
                        # job pending
                        if not (worker.poll () or worker.execute_next ()):
                            # If not the case free the worker slot
                            active_workers -= 1
                            self.workers[slot] = None

                    sleep (poll_sleep)

                if dyn_poll_interval:
                    poll_sleep = compute_next_dyn_poll(poll_counter,
                                                       poll_sleep)

        except StopIteration:
            # All the tests are finished.
            while active_workers > 0:
                for slot, worker in enumerate (self.workers):
                    if not (worker is None
                            or worker.poll ()
                            or worker.execute_next ()):
                        active_workers -= 1
                        self.workers[slot] = None
                    sleep (0.1)

    def abort (self):
        """Abort the loop"""
        if self.abort_file is not None and os.path.isfile (self.abort_file):
            for worker in self.workers:
                if worker is not None:
                    worker.wait ()

def compute_next_dyn_poll (poll_counter, poll_sleep):
    """Adjust the polling delay"""
    # if two much polling is done, the loop might consume too
    # much resources. In the opposite case, we might wait too
    # much to launch new jobs. Adjust accordingly.
    if poll_counter > 8 and poll_sleep < 1.0:
        poll_sleep *= 1.25
        logger.debug ('Increase poll interval to %d' % poll_sleep)
    elif poll_sleep > 0.0001:
        poll_sleep *= 0.75
        logger.debug ('Decrease poll interval to %d' % poll_sleep)
    return poll_sleep

