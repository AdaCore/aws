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
calling build_cmd (testid). Once a test is finished the function
collect_result will be called with test id, and process (a
gnatpython.ex.Run object) as parameters. Both build_cmd and
collect_result are user defined functions.

Note also that from the user point view there is no parallelism to handle.
The two user defined function build_cmd and collect_result are called
sequentially.
"""

import logging
import os
from time import sleep

logger = logging.getLogger ('gnatpython.mainloop')

class Worker (object):
    def __init__ (self, items, build_cmd, collect_result):
        """Worker constructor

        PARAMETERS
          items: item or list of items to be run by the worker
          build_cmd: command builder function (see MainLoop doc)
          collect_result: result processing function (see MailLoop doc)

        RETURN VALUE
          a Worker instance

        REMARKS
          None
        """
        self.build_cmd      = build_cmd
        self.collect_result = collect_result

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

            self.current_process = self.build_cmd (self.current_job)
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
        self.collect_result (self.current_job, self.current_process)
        self.current_process = None
        self.current_job = None

class MainLoop (object):
    def __init__ (self,
                  item_list,
                  build_cmd,
                  collect_result,
                  parallelism=1,
                  abort_file=None,
                  dyn_poll_interval=True):
        """Launch loop

        PARAMETERS
          item_list: a list of jobs
          build_cmd: a function that takes a job identifier as argument
                     and return the spawned process (ex.Run object). Note that
                     if you want to take advantage of the parallelism the
                     spawned process should be launched in background (ie with
                     bg=True when using ex.Run)
          collect_result: a function called when a job is finished. The
                     prototype should be func (name, process)
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
        self.workers            = [None] * parallelism
        self.item_list          = item_list
        self.iterator           = self.item_list.__iter__ ()
        self.active_workers     = 0
        self.build_cmd          = build_cmd
        self.collect_result     = collect_result
        self.max_active_workers = parallelism
        self.poll_sleep         = 0.1

        try:
            while True:
                # Check for abortion
                if abort_file is not None and os.path.isfile (abort_file):
                    logger.info ('Aborting: file %s has been found'
                                 % abort_file)
                    raise StopIteration

                # Find free workers
                for i, w in enumerate (self.workers):
                    if w is None:
                        # a worker slot is free so use it for next job
                        logger.debug ('Active worker %d' % i)
                        next_job = self.iterator.next ()
                        self.workers[i] = Worker (next_job,
                                                  self.build_cmd,
                                                  self.collect_result)
                        self.active_workers += 1

                poll_counter = 0
                logger.debug ('Wait for free worker')
                while self.active_workers >= self.max_active_workers:
                    # All worker are occupied so wait for one to finish
                    poll_counter += 1
                    for i, w in enumerate (self.workers):
                        # Test if the worker is still active and have more
                        # job pending
                        if not (w.poll () or w.execute_next ()):
                            # If not the case free the worker slot
                            self.active_workers -= 1
                            self.workers[i] = None

                    sleep (self.poll_sleep)

                if dyn_poll_interval:
                    # if two much polling is done, the loop might consume too
                    # much resources. In the opposite case, we might wait too
                    # much to launch new jobs. Adjust accordingly.
                    if poll_counter > 8 and self.poll_sleep < 1.0:
                        self.poll_sleep *= 1.25
                        logger.debug ('Increase poll interval to %d'
                                      % self.poll_sleep)
                    elif self.poll_sleep > 0.0001:
                        self.poll_sleep *= 0.75
                        logger.debug ('Decrease poll interval to %d'
                                      % self.poll_sleep)

        except StopIteration:
            # The loop has been aborted or all the tests are finished.

            if abort_file is not None and os.path.isfile (abort_file):
                for worker in self.workers:
                    if worker is not None:
                        worker.wait ()
            else:
                while self.active_workers > 0:
                    for i, w in enumerate (self.workers):
                        if not (w is None or w.poll () or w.execute_next ()):
                            self.active_workers -= 1
                            self.workers[i] = None
                    sleep (0.1)
