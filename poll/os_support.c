/***************************************************************************/
/*                             Ada Web Server                              */
/*                                                                         */
/*                       Copyright (C) 2009, AdaCore                       */
/*                                                                         */
/* This library is free software; you can redistribute it and/or modify    */
/* it under the terms of the GNU General Public License as published by    */
/* the Free Software Foundation; either version 2 of the License, or (at   */
/* your option) any later version.                                         */
/*                                                                         */
/* This library is distributed in the hope that it will be useful, but     */
/* WITHOUT ANY WARRANTY; without even the implied warranty of              */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       */
/* General Public License for more details.                                */
/*                                                                         */
/* You should have received a copy of the GNU General Public License       */
/* along with this library; if not, write to the Free Software Foundation, */
/* Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          */
/*                                                                         */
/* As a special exception, if other files instantiate generics from this   */
/* unit, or you link this unit with other files to produce an executable,  */
/* this  unit  does not  by itself cause  the resulting executable to be   */
/* covered by the GNU General Public License. This exception does not      */
/* however invalidate any other reasons why the executable file  might be  */
/* covered by the  GNU Public License.                                     */
/***************************************************************************/

/* Wrapper around some C macros */

#if defined(__vxworks)
#include <vxWorks.h>
#else
#if defined(_WIN32)
#include <winsock.h>
#else
#include <sys/socket.h>
#endif
#endif

#if !defined (_WIN32)
#define SOCKET int
#endif

#include <string.h>

int
__aws_is_socket_in_set (SOCKET socket, fd_set *set)
{
  return FD_ISSET (socket, set);
}

void
__aws_set_socket_in_set (SOCKET socket, fd_set *set)
{
  FD_SET (socket, set);
}

void
__aws_clear_socket_set (fd_set *set)
{
  FD_ZERO (set);
}
