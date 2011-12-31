/***************************************************************************
 *                             Ada Web Server                              *
 *                                                                         *
 *                    Copyright (C) 2003-2012, AdaCore                     *
 *                                                                         *
 * This library is free software;  you can redistribute it and/or modify   *
 * it under terms of the  GNU General Public License  as published by the  *
 * Free Software  Foundation;  either version 3,  or (at your  option) any *
 * later version. This library is distributed in the hope that it will be  *
 * useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    *
 *                                                                         *
 * As a special exception under Section 7 of GPL version 3, you are        *
 * granted additional permissions described in the GCC Runtime Library     *
 * Exception, version 3.1, as published by the Free Software Foundation.   *
 *                                                                         *
 * You should have received a copy of the GNU General Public License and   *
 * a copy of the GCC Runtime Library Exception along with this program;    *
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   *
 * <http://www.gnu.org/licenses/>.                                         *
 *                                                                         *
 *  As a special exception, if other files instantiate generics from this  *
 *  unit, or you link this unit with other files to produce an executable, *
 *  this  unit  does not  by itself cause  the resulting executable to be  *
 *  covered by the GNU General Public License. This exception does not     *
 *  however invalidate any other reasons why the executable file  might be *
 *  covered by the  GNU Public License.                                    *
 ***************************************************************************/

/* Replacement of the not thread safe Win32 gai_strerror inlined in the
   MSVC ws2tcpip.h. Note that ws2tcpip.h from Mingw (at least from GCC 3.4.1)
   have only declaration of the gai_strerror. */

#include <ws2tcpip.h>

char * AWS_gai_strerror (int ecode)
{
  switch (ecode)
    {
    case EAI_AGAIN:
      return "Temporary failure in name resolution.";
    case EAI_BADFLAGS:
      return "Invalid value for ai_flags.";
    case EAI_FAIL:
      return "Nonrecoverable failure in name resolution.";
    case EAI_FAMILY:
      return "The ai_family member is not supported.";
    case EAI_MEMORY:
      return "Memory allocation failure.";
    case EAI_NODATA:
      return "No address associated with nodename.";
#ifndef __MINGW64_VERSION_MAJOR
    /* with mingw64 runtime EAI_NODATA and EAI_NONAME have the same value.
       This applies to both win32 and win64 */
    case EAI_NONAME:
      return "Neither nodename nor servname provided, or not known.";
#endif
    case EAI_SERVICE:
      return "The servname parameter is not supported for ai_socktype.";
    case EAI_SOCKTYPE:
      return "The ai_socktype member is not supported.";
    default:
      return "Unknown error.";
    }
}

/* Windows does not have socket error code to error message convertion */

char * socket_strerror (int ecode)
{
  switch (ecode)
  {
    case WSAEINTR:
      return "Interrupted system call";
    case WSAEBADF:
      return "Bad file number";
    case WSAEACCES:
      return "Permission denied";
    case WSAEFAULT:
      return "Bad address";
    case WSAEINVAL:
      return "Invalid argument";
    case WSAEMFILE:
      return "Too many open files";
    case WSAEWOULDBLOCK:
      return "Operation would block";
    case WSAEINPROGRESS:
      return "Operation now in progress";
    case WSAEALREADY:
      return "Operation already in progress";
    case WSAENOTSOCK:
      return "Socket operation on nonsocket";
    case WSAEDESTADDRREQ:
      return "Destination address required";
    case WSAEMSGSIZE:
      return "Message too long";
    case WSAEPROTOTYPE:
      return "Protocol wrong type for socket";
    case WSAENOPROTOOPT:
      return "Protocol not available";
    case WSAEPROTONOSUPPORT:
      return "Protocol not supported";
    case WSAESOCKTNOSUPPORT:
      return "Socket type not supported";
    case WSAEOPNOTSUPP:
      return "Operation not supported on socket";
    case WSAEPFNOSUPPORT:
      return "Protocol family not supported";
    case WSAEAFNOSUPPORT:
      return "Address family not supported by protocol family";
    case WSAEADDRINUSE:
      return "Address already in use";
    case WSAEADDRNOTAVAIL:
      return "Cannot assign requested address";
    case WSAENETDOWN:
      return "Network is down";
    case WSAENETUNREACH:
      return "Network is unreachable";
    case WSAENETRESET:
      return "Network dropped connection on reset";
    case WSAECONNABORTED:
      return "Software caused connection abort";
    case WSAECONNRESET:
      return "Connection reset by peer";
    case WSAENOBUFS:
      return "No buffer space available";
    case WSAEISCONN :
      return "Socket is already connected";
    case WSAENOTCONN:
      return "Socket is not connected";
    case WSAESHUTDOWN:
      return "Cannot send after socket shutdown";
    case WSAETOOMANYREFS:
      return "Too many references: cannot splice";
    case WSAETIMEDOUT:
      return "Connection timed out";
    case WSAECONNREFUSED:
      return "Connection refused";
    case WSAELOOP:
      return "Too many levels of symbolic links";
    case WSAENAMETOOLONG:
      return "File name too long";
    case WSAEHOSTDOWN:
      return "Host is down";
    case WSAEHOSTUNREACH:
      return "No route to host";
    case WSASYSNOTREADY:
      return "Returned by WSAStartup(), indicating that "
                     "the network subsystem is unusable";
    case WSAVERNOTSUPPORTED:
      return "Returned by WSAStartup(), indicating that "
                     "the Windows Sockets DLL cannot support "
                     "this application";
    case WSANOTINITIALISED:
      return "Winsock not initialized";
    case WSAEDISCON:
      return "Disconnected";
    case HOST_NOT_FOUND:
      return "Host not found";
    case TRY_AGAIN:
      return "Nonauthoritative host not found";
    case NO_RECOVERY:
      return "Nonrecoverable error";
    case NO_DATA:
      return "Valid name, no data record of requested type";
    default:
      return NULL;
    }
}
