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

#include <windows.h>
#include <stdio.h>
#include <tlhelp32.h>

static int child_finished;

usage ()
{
  printf ("Usage:\n");
  printf ("   rlimit seconds command [args]\n");
  exit (1);
}

#define MAX_CHILDREN_NUMBER 1024

kill_process_and_children (DWORD process_id)
{
   PROCESSENTRY32 current_process;
   HANDLE deleted_process_handle;
   int j;
   int has_next;
   HANDLE tool_help_snapshot;

   current_process.dwSize = sizeof(PROCESSENTRY32);
   tool_help_snapshot = CreateToolhelp32Snapshot (TH32CS_SNAPPROCESS, 0);
   has_next = Process32First (tool_help_snapshot, &current_process);

   while (has_next)
   {
      if (current_process.th32ParentProcessID == process_id)
      {
         kill_process_and_children (current_process.th32ProcessID);
      }

      has_next = Process32Next (tool_help_snapshot, &current_process);
   }

   deleted_process_handle = OpenProcess
      (PROCESS_TERMINATE, FALSE, process_id);
   GenerateConsoleCtrlEvent (CTRL_BREAK_EVENT, process_id);
   TerminateProcess (deleted_process_handle, 1);
   CloseHandle (deleted_process_handle);
}

main (argc, argv)
     int argc;
     char *argv[];
{
  BOOL result;
  STARTUPINFO SI;
  PROCESS_INFORMATION PI;
  SECURITY_ATTRIBUTES SA;

  char full_command[2000];
  int k, seconds, child_finished;

  /* We need at least 3 args.  */
  if (argc < 3)
    usage ();

  /* argv[0] = .../rlimit
     argv[1] = seconds
     argv[2] = command
     argv[3] = args */

  /* Startup info.  */
  SI.cb          = sizeof (STARTUPINFO);
  SI.lpReserved  = NULL;
  SI.lpReserved2 = NULL;
  SI.lpDesktop   = NULL;
  SI.cbReserved2 = 0;
  SI.lpTitle     = NULL;
  SI.dwFlags     = 0;
  SI.wShowWindow = SW_HIDE;

  /* Security attributes.  */
  SA.nLength = sizeof (SECURITY_ATTRIBUTES);
  SA.bInheritHandle = TRUE;
  SA.lpSecurityDescriptor = NULL;

  /* Prepare the command string. */
  strcpy (full_command, argv[2]);
  strcat (full_command, " ");

  k = 3;
  while (argv[k])
    {
      strcat (full_command, argv[k]);
      strcat (full_command, " ");
      k++;
    }

  result = CreateProcess (NULL, (char *) full_command, &SA, NULL, TRUE,
                          NORMAL_PRIORITY_CLASS | CREATE_NEW_PROCESS_GROUP,
                          NULL, NULL, &SI, &PI);

  /* parent sleeps
     wake up when the sleep call returns or when
     SIGCHLD is received  */

  seconds = atoi (argv[1]);
  child_finished
    = WaitForSingleObject (PI.hProcess, seconds * 1000) == WAIT_OBJECT_0;

  if (child_finished)
  {
     DWORD exit_code;

     if (GetExitCodeProcess (PI.hProcess, &exit_code))
     {
        exit (exit_code);
     }
     else
     {
        exit (0);
     }
  }

  /* Sleep call returns, we must kill the child. */
  kill_process_and_children (PI.dwProcessId);
  CloseHandle (PI.hProcess);
  fprintf (stderr, "rlimit: Real time limit exceeded\n");
  exit (2);
}
