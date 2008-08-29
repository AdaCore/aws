/*****************************************************************************
 *                                                                           *
 *                                Rlimit                                     *
 *                                                                           *
 *                    Copyright (C) 1996-2008, AdaCore                       *
 *                                                                           *
 *  This library is free software; you can redistribute it and/or modify     *
 *  it under the terms of the GNU General Public License as published by     *
 *  the Free Software Foundation; either version 2 of the License, or (at    *
 *  your option) any later version.                                          *
 *                                                                           *
 *  This library is distributed in the hope that it will be useful, but      *
 *  WITHOUT ANY WARRANTY; without even the implied warranty of               *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU        *
 *  General Public License for more details.                                 *
 *                                                                           *
 *  You should have received a copy of the GNU General Public License        *
 *  along with this library; if not, write to the Free Software Foundation,  *
 *  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.           *
 *                                                                           *
 *  As a special exception, if other files instantiate generics from this    *
 *  unit, or you link this unit with other files to produce an executable,   *
 *  this  unit  does not  by itself cause  the resulting executable to be    *
 *  covered by the GNU General Public License. This exception does not       *
 *  however invalidate any other reasons why the executable file  might be   *
 *  covered by the  GNU Public License.                                      *
 ****************************************************************************/

/* rlimit - limit the execution time of a command

   Usage:
      rlimit seconds command [args]   */

#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <errno.h>

static char const scmid[] = "$Id$";

void
usage (void)
{
  printf ("Usage:\n");
  printf ("   rlimit seconds command [args]\n");
  exit (1);
}

/* Handler for SIGCHLD */

static int pid = 0;
int status = 0;
int return_status = 0;

void
reapchild (int nsig)
{
  int delay;

  if (pid > 0) {
    int rc;

    /*
     * Wait for the (only) child process. Since we have received SIGCHLD,
     * we know that this will not return ECHILD or 0. Note that waitpid(3)
     * won't report information for indirect descendants, but only for direct
     * child processes, in any case.
     */

    rc = waitpid (pid, &status, WNOHANG);
    if (rc < 0) {
       perror ("waitpid");
       return;
    }

    /* Get child process exit status */

    if (WIFEXITED (status) != 0) {
       return_status = WEXITSTATUS (status);
    } else {
       return_status = -125;
       /* ??? This junk value is invalid. */
    }

    /*
     * Check for remaining processes in the child group. Give them
     * 5 seconds to die gracefully.
     */

    delay = 5;
    while (delay > 0 && !(kill (-pid, 0) == -1 && errno == ESRCH)) {
      sleep (1);
      --delay;
    }

    if (delay == 0) {

       /* Now kill any non-terminated children remaining in group. */

       kill (-pid, SIGTERM);
       sleep (1);
       kill (-pid, SIGKILL);
    }

    /* Report exit status from child process to caller. */

    exit (return_status);

  } else {
    /* Never happens (the child process does an execve and does not fork). */
    exit (0);
  }
}

int
main (int argc, char **argv)
{
  sigset_t block_cld;

  /* we need at least 3 args */
  if (argc < 3)
    usage ();

  /* argv[0] = .../rlimit
     argv[1] = seconds
     argv[2] = command
     argv[3] = args */

  /*
   * When the child process exits early, SIGCHLD might be emitted before the
   * pid variable is set in the parent process. On the other hand, we do want
   * to receive the signal so we have a chance to kill any other process it
   * might have spawned in the meantime. So, we establish the SIGCHLD handler
   * early, and block SIGCHLD until pid has been set.
   */

  signal (SIGCHLD, reapchild);

  sigemptyset(&block_cld);
  sigaddset(&block_cld, SIGCHLD);
  sigprocmask(SIG_BLOCK, &block_cld, NULL);

  pid = fork ();
  switch (pid) {
    case -1:
      perror ("fork");
      exit (3);

    case 0:
      /* first unblock SIGCHLD */
      sigprocmask(SIG_UNBLOCK, &block_cld, NULL);

      /* child exec the command in a new process group */
      if (setpgid (0, 0)) {
        perror ("setpgid");
        exit (4);
      }

      execvp ((const char *) argv[2], (char *const *) &argv[2]);
      perror ("execvp");
      exit (5);

    default: {
      /* parent sleeps
	 wake up when the sleep call returns or when
	 SIGCHLD is received */
      int seconds = atoi (argv[1]);

      /* pid variable is now set correctly so unblock SIGCHLD */
      sigprocmask(SIG_UNBLOCK, &block_cld, NULL);

      sleep (seconds);

      /* Sleep call returns. The time limit has been exceeded and we
	 must kill the child.

	 Print the diagnostic first: On some systems (eg. LynxOS) the
	 handler for SIGCHLD may interrupt write(2) and garble the
	 message. */

      fprintf (stderr, "rlimit: Real time limit exceeded\n");
      fflush (stderr);

      kill (-pid, SIGTERM);
      sleep (1);
      kill (-pid, SIGKILL);
      exit (2);
    }
  }
  return return_status;
}
