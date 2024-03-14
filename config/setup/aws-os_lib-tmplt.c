/*
------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2012-2020, AdaCore                     --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

pragma Style_Checks ("M32766");
--  Allow long lines

*/

/**
 **  This template file is used while building the AWS library to
 **  generate package AWS.OS_Lib (aws-os_lib.ads).
 **
 **  The generation process is:
 **  1. the platform-independent extraction tool xoscons is built with the
 **     base native compiler
 **  2. this template is processed by the cross C compiler to produce
 **     a list of constant values
 **  3. the comments in this template and the list of values are processed
 **     by xoscons to generate aws-os_lib.ads.
 **
 **  Any comment occurring in this file whose start and end markers are on
 **  a line by themselves (see above) is copied verbatim to aws-os_lib.ads.
 **  All other comments are ignored. Note that the build process first passes
 **  this file through the C preprocessor, so comments that occur in a section
 **  that is conditioned by a #if directive will be copied to the output only
 **  when it applies.
 **
 **  The procedure assumes that the template can be compiled by the newly-
 **  built cross compiler. It uses markup produced in the (pseudo-)assembly
 **  listing:
 **
 **     $target-gcc -DTARGET=\"$target\" -C -E \
 **        aws-os_lib-tmplt.c > aws-os_lib-tmplt.i
 **     $target-gcc -S aws-os_lib-tmplt.i
 **     xoscons aws-os_lib
 **
 **  It is also possible to generate aws-os_lib.ads on a native environment:
 **
 **     gcc -DTARGET=\"$target\" -C -E aws-os_lib-tmplt.c > aws-os_lib-tmplt.i
 **     gcc -S aws-os_lib-tmplt.i
 **     xoscons aws-os_lib
 **/

#ifndef TARGET
# error Please define TARGET
#endif

/* Feature macro definitions */

#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <fcntl.h>
#include <time.h>
#include <errno.h>

#if defined(__vxworks)

/**
 ** For VxWorks, always include vxWorks.h
 **/

# include <vxWorks.h>
# include "sockLib.h"
# include "inetLib.h"
# include "stdioLib.h"
# include "strLib.h"
# include "hostLib.h"
# include "ioLib.h"
# include <limits.h>

#define SHUT_RD   0
#define SHUT_WR   1
#define SHUT_RDWR 2
#endif /* __vxworks */

#ifdef HAVE_TERMIOS
# include <termios.h>
#endif

#ifdef __APPLE__
# include <_types.h>
#endif

#ifdef _WIN32
# include <ws2tcpip.h>
# include <winsock2.h>
# include <windows.h>
# include <winerror.h>

#if !defined SHUT_RD && defined SD_RECEIVE
#define SHUT_RD SD_RECEIVE
#endif
#if !defined SHUT_WR && defined SD_SEND
#define SHUT_WR SD_SEND
#endif
#if !defined SHUT_RDWR && defined SD_BOTH
#define SHUT_RDWR SD_BOTH
#endif

#define EADDRNOTAVAIL   WSAEADDRNOTAVAIL
#define ETIMEDOUT       WSAETIMEDOUT
#define ENOTCONN        WSAENOTCONN
#define EWOULDBLOCK     WSAEWOULDBLOCK
#define EINPROGRESS     WSAEINPROGRESS
#define ENOBUFS         WSAENOBUFS
#define ESHUTDOWN       WSAESHUTDOWN
#define ESOCKTNOSUPPORT WSAESOCKTNOSUPPORT
#define ETOOMANYREFS    WSAETOOMANYREFS
#define ECONNRESET      WSAECONNRESET
#define EADDRINUSE      WSAEADDRINUSE
#define EAFNOSUPPORT    WSAEAFNOSUPPORT
#define EALREADY        WSAEALREADY
#define ECONNABORTED    WSAECONNABORTED
#define ECONNREFUSED    WSAECONNREFUSED
#define EDESTADDRREQ    WSAEDESTADDRREQ
#define EHOSTDOWN       WSAEHOSTDOWN
#define EHOSTUNREACH    WSAEHOSTUNREACH
#define EISCONN         WSAEISCONN
#define ELOOP           WSAELOOP
#define EMSGSIZE        WSAEMSGSIZE
#define EPFNOSUPPORT    WSAEPFNOSUPPORT
#define EPROTONOSUPPORT WSAEPROTONOSUPPORT
#define EPROTOTYPE      WSAEPROTOTYPE
#define ENETDOWN        WSAENETDOWN
#define ENETRESET       WSAENETRESET
#define ENETUNREACH     WSAENETUNREACH
#define ENOPROTOOPT     WSAENOPROTOOPT
#define ENOTSOCK        WSAENOTSOCK
#define EOPNOTSUPP      WSAEOPNOTSUPP
#define EAI_SYSTEM      EAI_AGAIN

#else /* _WIN32 */

#ifndef __vxworks
# include <sys/poll.h>
#endif

# include <sys/types.h>
# include <sys/socket.h>
# include <sys/ioctl.h>
# include <netdb.h>
# include <netinet/in.h>
# include <netinet/tcp.h>
# include <errno.h>
#endif /* _WIN32 */

#ifdef sun
# include <sys/filio.h>
#endif

#if defined (__NetBSD__)  \
 || defined (__OpenBSD__) \
 || defined (__FreeBSD__)
# include <sys/time.h>
#endif

#if defined (__QNX__)
# include <sys/select.h>
#endif

#define CND(name, comment) \
  asm volatile("\n->CND:%0:" #name ":%1:" comment \
  : : "i" (__LINE__), "i" ((int) name));
/* Decimal constant in the range of type "int" */

#define CNU(name, comment) \
  asm volatile("\n->CNU:%0:" #name ":%1:" comment \
  : : "i" (__LINE__), "i" ((int) name));
/* Decimal constant in the range of type "unsigned int" (note, assembler
 * always wants a signed int, we convert back in xoscons).
 */

#define CNS(name, comment) \
  asm volatile("\n->CNS:%0:" #name ":" name ":" comment \
  : : "i" (__LINE__));
/* General expression named number */

#define C(sname, type, value, comment) \
  asm volatile("\n->C:%0:" sname ":" #type ":" value ":" comment \
  : : "i" (__LINE__));
/* Typed constant */

#define TXT(text) \
  asm volatile("\n->TXT:%0:" text \
  : : "i" (__LINE__));
/* Freeform text */

#define CST(name,comment) C(#name,String,name,comment)

#define STR(x) STR1(x)
#define STR1(x) #x

int
main (void) {

/*
--  This package provides target dependent definitions of constant/types for
--  use by the AWS library. This package should not be directly with'd
--  by an application program.

--  This file is generated automatically, do not modify it by hand! Instead,
--  make changes to aws-os_lib-tmplt.c and rebuild the AWS library.
*/

TXT("--  This is the version for " TARGET)
TXT("")

/**
 **  The type definitions for struct hostent components uses Interfaces.C
 **/

TXT("with Interfaces.C.Strings;")
TXT("with System;")
#ifndef _WIN32
TXT("with GNAT.OS_Lib;")
#endif

/*

package AWS.OS_Lib is

   use Interfaces;
*/

/**
 **  General constants (all platforms)
 **/

/*

   ---------------------------------
   -- General platform parameters --
   ---------------------------------

   type OS_Type is (Windows, VMS, Other_OS);
*/
#if defined (__MINGW32__)
# define TARGET_OS "Windows"
#elif defined (__VMS)
# define TARGET_OS "VMS"
#else
# define TARGET_OS "Other_OS"
#endif
C("Target_OS", OS_Type, TARGET_OS, "")
/*
   pragma Warnings (Off, Target_OS);
   --  Suppress warnings on Target_OS since it is in general tested for
   --  equality with a constant value to implement conditional compilation,
   --  which normally generates a constant condition warning.

*/
#define Target_Name TARGET
CST(Target_Name, "")

#ifdef _WIN32
/*

   Executable_Extension : constant String    := ".exe";
   Directory_Separator  : constant Character := '\';
   Path_Separator       : constant Character := ';';
*/
#else
/*

   Executable_Extension : constant String    := "";
   Directory_Separator  : constant Character := '/';
   Path_Separator       : constant Character := ':';
*/
#endif

/*

   --  Sizes of various data types

*/

/**
 **  Note: the name of the following constant is recognized specially by
 **  xoscons (case sensitive).
 **/
#define SIZEOF_unsigned_int sizeof (unsigned int)
CND(SIZEOF_unsigned_int, "Size of unsigned int")

#define SIZEOF_fd_set (sizeof (fd_set))
CND(SIZEOF_fd_set, "fd_set");
CND(FD_SETSIZE, "Max fd value");

{
  const struct sockaddr_in sa;

#define SIZEOF_sin_family sizeof (sa.sin_family) * 8
CND(SIZEOF_sin_family, "Size of sa.sin_family");

#define SIN_FAMILY_OFFSET (uintptr_t)((uintptr_t)&sa.sin_family    \
                                    - (uintptr_t)&sa)
/*NOGEN*/ CND(SIN_FAMILY_OFFSET, "sin_family offset in record");
}

{
#ifndef AI_PASSIVE
#define AI_FAMILY_OFFSET -7
#define AI_ADDR_OFFSET -3
#define AI_CANONNAME_OFFSET -2

#else
  const struct addrinfo ai;

#define AI_FAMILY_OFFSET (uintptr_t)((uintptr_t)&ai.ai_family \
                                     - (uintptr_t)&ai)
#define AI_CANONNAME_OFFSET (uintptr_t)((uintptr_t)&ai.ai_canonname \
                                        - (uintptr_t)&ai)
#define AI_ADDR_OFFSET (uintptr_t)((uintptr_t)&ai.ai_addr - (uintptr_t)&ai)
#endif

/*NOGEN*/ CND(AI_FAMILY_OFFSET, "???");
/*NOGEN*/ CND(AI_CANONNAME_OFFSET, "???");
/*NOGEN*/ CND(AI_ADDR_OFFSET, "???");
}

#if defined(_WIN32) || defined(__vxworks)
#define SIZEOF_nfds_t sizeof (int) * 8
#else
#define SIZEOF_nfds_t sizeof (nfds_t) * 8
#endif
CND(SIZEOF_nfds_t, "Size of nfds_t");

{
#if defined(_WIN32) || defined(__vxworks)
#define SIZEOF_pollfd_events sizeof (short) * 8
#else
const struct pollfd v_pollfd;
#define SIZEOF_pollfd_events sizeof (v_pollfd.events) * 8
#endif
CND(SIZEOF_pollfd_events, "Size of pollfd.events");

#if defined(_WIN32)
#define SIZEOF_fd_type sizeof (SOCKET) * 8
#else
#if defined(__vxworks)
#define SIZEOF_fd_type sizeof (int) * 8
#else
#define SIZEOF_fd_type sizeof (v_pollfd.fd) * 8
#endif
#endif
CND(SIZEOF_fd_type, "Size of socket fd");
}

#if defined(_WIN32) || defined(__vxworks)
#define SIZEOF_socklen_t sizeof (size_t) * 8
#else
#define SIZEOF_socklen_t sizeof (socklen_t) * 8
#endif
CND(SIZEOF_socklen_t, "Size of socklen_t");

{
  const struct timeval tv;
/*

   --  Sizes (in bytes) of the components of struct timeval
*/
#define SIZEOF_tv_sec (sizeof tv.tv_sec) * 8
CND(SIZEOF_tv_sec, "tv_sec")
#define SIZEOF_tv_usec (sizeof tv.tv_usec) * 8
CND(SIZEOF_tv_usec, "tv_usec")
}

/*

   --  Poll values

*/

#if defined(_WIN32) || defined(__vxworks)
#ifndef POLLIN
#define POLLIN 1
#endif

#ifndef POLLPRI
#define POLLPRI 2
#endif

#ifndef POLLOUT
#define POLLOUT 4
#endif

#ifndef POLLERR
#define POLLERR 8
#endif

#ifndef POLLHUP
#define POLLHUP 16
#endif

#ifndef POLLNVAL
#define POLLNVAL 32
#endif
#endif

CND(POLLIN, "There is data to read");
CND(POLLPRI, "Urgent data to read");
CND(POLLOUT, "Writing will not block");
CND(POLLERR, "Error (output only)");
CND(POLLHUP, "Hang up (output only)");
CND(POLLNVAL, "Invalid request");

/*

   -----------------
   -- Fcntl flags --
   -----------------

*/

#ifndef FNDELAY
# define FNDELAY -1
#endif
CND(FNDELAY, "Nonblocking")

/*

   ----------------------
   -- Ioctl operations --
   ----------------------

*/

#ifndef FIONBIO
# define FIONBIO -1
#endif
CND(FIONBIO, "Set/clear non-blocking io")

#ifndef FIONREAD
# define FIONREAD -1
#endif
CND(FIONREAD, "How many bytes to read")

#ifndef FIONWRITE
# if defined(SIOCOUTQ)
#  define FIONWRITE SIOCOUTQ
# elif defined(TIOCOUTQ)
#  define FIONWRITE TIOCOUTQ
# else
#  define FIONWRITE -1
# endif
#endif
CND(FIONWRITE, "How many bytes in the send queue")

#ifndef FIONSPACE
# define FIONSPACE -1
#endif
CND(FIONSPACE, "Free space in the send queue")

/*

   ---------------------------------------
   -- getaddrinfo getnameinfo constants --
   ---------------------------------------

*/

#ifndef AI_PASSIVE
# define AI_PASSIVE -1
#endif
CND(AI_PASSIVE, "NULL nodename for accepting")

#ifndef AI_CANONNAME
# define AI_CANONNAME -1
#endif
CND(AI_CANONNAME, "Get the host official name")

#ifndef AI_NUMERICSERV
# define AI_NUMERICSERV -1
#endif
CND(AI_NUMERICSERV, "Service is a numeric string")

#ifndef AI_NUMERICHOST
# define AI_NUMERICHOST -1
#endif
CND(AI_NUMERICHOST, "Node is a numeric IP address")

#ifndef EAI_SYSTEM
# define EAI_SYSTEM -1
#endif
CND(EAI_SYSTEM, "Check errno for details")

#ifndef NI_NUMERICHOST
# define NI_NUMERICHOST -1
#endif
CND(NI_NUMERICHOST, "Numeric form of the hostname")

/*

   ------------------
   -- Errno values --
   ------------------

   --  The following constants are defined from <errno.h>

*/
#ifndef EAGAIN
# define EAGAIN -1
#endif
CND(EAGAIN, "Try again")

#ifndef ENOENT
# define ENOENT -1
#endif
CND(ENOENT, "File not found")

#ifndef ENOMEM
# define ENOMEM -1
#endif
CND(ENOMEM, "Out of memory")

#ifdef __MINGW32__
/*

   --  The following constants are defined from <winsock2.h> (WSA*)

*/
#endif

#ifndef EADDRNOTAVAIL
# define EADDRNOTAVAIL -1
#endif
CND(EADDRNOTAVAIL, "Cannot assign address")

#ifndef EINPROGRESS
# define EINPROGRESS -1
#endif
CND(EINPROGRESS, "Operation now in progress")

#ifndef EINTR
# define EINTR -1
#endif
CND(EINTR, "Interrupted system call")

#ifndef EINVAL
# define EINVAL -1
#endif
CND(EINVAL, "Invalid argument")

#ifndef ENAMETOOLONG
# define ENAMETOOLONG -1
#endif
CND(ENAMETOOLONG, "Name too long")

#ifndef ENOBUFS
# define ENOBUFS -1
#endif
CND(ENOBUFS, "No buffer space available")

#ifndef ENOTCONN
# define ENOTCONN -1
#endif
CND(ENOTCONN, "Socket not connected")

#ifndef ESHUTDOWN
# define ESHUTDOWN -1
#endif
CND(ESHUTDOWN, "Cannot send once shutdown")

#ifndef ESOCKTNOSUPPORT
# define ESOCKTNOSUPPORT -1
#endif
CND(ESOCKTNOSUPPORT, "Socket type not supported")

#ifndef ETIMEDOUT
# define ETIMEDOUT -1
#endif
CND(ETIMEDOUT, "Connection timed out")

#ifndef ETOOMANYREFS
# define ETOOMANYREFS -1
#endif
CND(ETOOMANYREFS, "Too many references")

#ifndef EWOULDBLOCK
# define EWOULDBLOCK -1
#endif
CND(EWOULDBLOCK, "Operation would block")

#ifndef ECONNRESET
# define ECONNRESET -1
#endif
CND(ECONNRESET, "Connection reset by peer")

#ifndef EACCES
# define EACCES -1
#endif
CND(EACCES, "Permission denied")

#ifndef EADDRINUSE
# define EADDRINUSE -1
#endif
CND(EADDRINUSE, "Address already in use")

#ifndef EAFNOSUPPORT
# define EAFNOSUPPORT -1
#endif
CND(EAFNOSUPPORT, "Addr family not supported")

#ifndef EALREADY
# define EALREADY -1
#endif
CND(EALREADY, "Operation in progress")

#ifndef EBADF
# define EBADF -1
#endif
CND(EBADF, "Bad file descriptor")

#ifndef ECONNABORTED
# define ECONNABORTED -1
#endif
CND(ECONNABORTED, "Connection aborted")

#ifndef ECONNREFUSED
# define ECONNREFUSED -1
#endif
CND(ECONNREFUSED, "Connection refused")

#ifndef EDESTADDRREQ
# define EDESTADDRREQ -1
#endif
CND(EDESTADDRREQ, "Destination addr required")

#ifndef EFAULT
# define EFAULT -1
#endif
CND(EFAULT, "Bad address")

#ifndef EHOSTDOWN
# define EHOSTDOWN -1
#endif
CND(EHOSTDOWN, "Host is down")

#ifndef EHOSTUNREACH
# define EHOSTUNREACH -1
#endif
CND(EHOSTUNREACH, "No route to host")

#ifndef EIO
# define EIO -1
#endif
CND(EIO, "Input output error")

#ifndef EISCONN
# define EISCONN -1
#endif
CND(EISCONN, "Socket already connected")

#ifndef ELOOP
# define ELOOP -1
#endif
CND(ELOOP, "Too many symbolic links")

#ifndef EMFILE
# define EMFILE -1
#endif
CND(EMFILE, "Too many open files")

#ifndef EMSGSIZE
# define EMSGSIZE -1
#endif
CND(EMSGSIZE, "Message too long")

#ifndef EPIPE
# define EPIPE -1
#endif
CND(EPIPE, "Broken pipe")

#ifndef EPFNOSUPPORT
# define EPFNOSUPPORT -1
#endif
CND(EPFNOSUPPORT, "Unknown protocol family")

#ifndef EPROTONOSUPPORT
# define EPROTONOSUPPORT -1
#endif
CND(EPROTONOSUPPORT, "Unknown protocol")

#ifndef EPROTOTYPE
# define EPROTOTYPE -1
#endif
CND(EPROTOTYPE, "Unknown protocol type")

#ifndef ENETDOWN
# define ENETDOWN -1
#endif
CND(ENETDOWN, "Network is down")

#ifndef ENETRESET
# define ENETRESET -1
#endif
CND(ENETRESET, "Disconn. on network reset")

#ifndef ENETUNREACH
# define ENETUNREACH -1
#endif
CND(ENETUNREACH, "Network is unreachable")

#ifndef ENOPROTOOPT
# define ENOPROTOOPT -1
#endif
CND(ENOPROTOOPT, "Protocol not available")

#ifndef ENOTSOCK
# define ENOTSOCK -1
#endif
CND(ENOTSOCK, "Operation on non socket")

#ifndef EOPNOTSUPP
# define EOPNOTSUPP -1
#endif
CND(EOPNOTSUPP, "Operation not supported")

/*

   --------------
   -- Families --
   --------------

*/

#ifndef AF_INET
# define AF_INET -1
#endif
CND(AF_INET, "IPv4 address family")

/**
 ** RTEMS lies and defines AF_INET6 even though there is no IPV6 support.
 ** Its TCP/IP stack is in transition.  It has newer .h files but no IPV6 yet.
 **/
#if defined(__rtems__)
# undef AF_INET6
#endif

#ifndef AF_INET6
# define AF_INET6 -1
#else
# define HAVE_AF_INET6 1
#endif
CND(AF_INET6, "IPv6 address family")

#ifndef AF_UNSPEC
# define AF_UNSPEC -1
#else
# define HAVE_AF_UNSPEC 1
#endif
CND(AF_UNSPEC, "Unspecified address family")

/*

   ------------------
   -- Socket modes --
   ------------------

*/

#ifndef SOCK_STREAM
# define SOCK_STREAM -1
#endif
CND(SOCK_STREAM, "Stream socket")

#ifndef SOCK_DGRAM
# define SOCK_DGRAM -1
#endif
CND(SOCK_DGRAM, "Datagram socket")

/*

   -----------------
   -- Host errors --
   -----------------

*/

#ifndef HOST_NOT_FOUND
# define HOST_NOT_FOUND -1
#endif
CND(HOST_NOT_FOUND, "Unknown host")

#ifndef TRY_AGAIN
# define TRY_AGAIN -1
#endif
CND(TRY_AGAIN, "Host name lookup failure")

#ifndef NO_DATA
# define NO_DATA -1
#endif
CND(NO_DATA, "No data record for name")

#ifndef NO_RECOVERY
# define NO_RECOVERY -1
#endif
CND(NO_RECOVERY, "Non recoverable errors")

/*

   --------------------
   -- Shutdown modes --
   --------------------

*/

#ifndef SHUT_RD
# define SHUT_RD -1
#endif
CND(SHUT_RD, "No more recv")

#ifndef SHUT_WR
# define SHUT_WR -1
#endif
CND(SHUT_WR, "No more send")

#ifndef SHUT_RDWR
# define SHUT_RDWR -1
#endif
CND(SHUT_RDWR, "No more recv/send")

/*

   ---------------------
   -- Protocol levels --
   ---------------------

*/

#ifndef SOL_SOCKET
# define SOL_SOCKET -1
#endif
CND(SOL_SOCKET, "Options for socket level")

#ifndef IPPROTO_IP
# define IPPROTO_IP -1
#endif
CND(IPPROTO_IP, "Dummy protocol for IP")

#ifndef IPPROTO_IPV6
# define IPPROTO_IPV6 -1
#endif
CND(IPPROTO_IPV6, "IPv6 socket option level")

#ifndef IPPROTO_UDP
# define IPPROTO_UDP -1
#endif
CND(IPPROTO_UDP, "UDP")

#ifndef IPPROTO_TCP
# define IPPROTO_TCP -1
#endif
CND(IPPROTO_TCP, "TCP")

/*

   -------------------
   -- Request flags --
   -------------------

*/

#ifndef MSG_OOB
# define MSG_OOB -1
#endif
CND(MSG_OOB, "Process out-of-band data")

#ifndef MSG_PEEK
# define MSG_PEEK -1
#endif
CND(MSG_PEEK, "Peek at incoming data")

#ifndef MSG_EOR
# define MSG_EOR -1
#endif
CND(MSG_EOR, "Send end of record")

#ifndef MSG_WAITALL
#ifdef __MINWGW32__
/* The value of MSG_WAITALL is 8.  Nevertheless winsock.h doesn't
   define it, but it is still usable as we link to winsock2 API.  */
# define MSG_WAITALL (1 << 3)
#else
# define MSG_WAITALL -1
#endif
#endif
CND(MSG_WAITALL, "Wait for full reception")

#ifndef MSG_NOSIGNAL
# define MSG_NOSIGNAL -1
#endif
CND(MSG_NOSIGNAL, "No SIGPIPE on send")

#ifdef __linux__
# define MSG_Forced_Flags "MSG_NOSIGNAL"
#else
# define MSG_Forced_Flags "0"
#endif
CNS(MSG_Forced_Flags, "")
/*
   --  Flags set on all send(2) calls
*/

/*

   --------------------
   -- Socket options --
   --------------------

*/

#ifndef TCP_NODELAY
# define TCP_NODELAY -1
#endif
CND(TCP_NODELAY, "Do not coalesce packets")

#ifndef SO_REUSEADDR
# define SO_REUSEADDR -1
#endif
CND(SO_REUSEADDR, "Bind reuse local address")

#ifndef SO_KEEPALIVE
# define SO_KEEPALIVE -1
#endif
CND(SO_KEEPALIVE, "Enable keep-alive msgs")

#ifndef SO_LINGER
# define SO_LINGER -1
#endif
CND(SO_LINGER, "Defer close to flush data")

#ifndef SO_BROADCAST
# define SO_BROADCAST -1
#endif
CND(SO_BROADCAST, "Can send broadcast msgs")

#ifndef SO_SNDBUF
# define SO_SNDBUF -1
#endif
CND(SO_SNDBUF, "Set/get send buffer size")

#ifndef SO_RCVBUF
# define SO_RCVBUF -1
#endif
CND(SO_RCVBUF, "Set/get recv buffer size")

#ifndef SO_SNDTIMEO
# define SO_SNDTIMEO -1
#endif
CND(SO_SNDTIMEO, "Emission timeout")

#ifndef SO_RCVTIMEO
# define SO_RCVTIMEO -1
#endif
CND(SO_RCVTIMEO, "Reception timeout")

#ifndef SO_ERROR
# define SO_ERROR -1
#endif
CND(SO_ERROR, "Get/clear error status")

#ifndef IP_MULTICAST_IF
# define IP_MULTICAST_IF -1
#endif
CND(IP_MULTICAST_IF, "Set/get mcast interface")

#ifndef IP_MULTICAST_TTL
# define IP_MULTICAST_TTL -1
#endif
CND(IP_MULTICAST_TTL, "Set/get multicast TTL")

#ifndef IP_MULTICAST_LOOP
# define IP_MULTICAST_LOOP -1
#endif
CND(IP_MULTICAST_LOOP, "Set/get mcast loopback")

#ifndef IP_ADD_MEMBERSHIP
# define IP_ADD_MEMBERSHIP -1
#endif
CND(IP_ADD_MEMBERSHIP, "Join a multicast group")

#ifndef IP_DROP_MEMBERSHIP
# define IP_DROP_MEMBERSHIP -1
#endif
CND(IP_DROP_MEMBERSHIP, "Leave a multicast group")

#ifndef IP_PKTINFO
# define IP_PKTINFO -1
#endif
CND(IP_PKTINFO, "Get datagram info")

#ifndef IPV6_V6ONLY
# define IPV6_V6ONLY -1
#endif
CND(IPV6_V6ONLY, "Restricted to IPv6 communications only")

/*

   --  Some types

   type nfds_t is mod 2 ** SIZEOF_nfds_t;
   for nfds_t'Size use SIZEOF_nfds_t;

   type FD_Type
     is range -(2 ** (SIZEOF_fd_type - 1)) .. 2 ** (SIZEOF_fd_type - 1) - 1;
   for FD_Type'Size use SIZEOF_fd_type;

   type Events_Type is mod 2 ** SIZEOF_pollfd_events;
   for Events_Type'Size use SIZEOF_pollfd_events;

   type socklen_t is mod 2 ** SIZEOF_socklen_t;
   for socklen_t'Size use SIZEOF_socklen_t;

   type timeval_tv_sec_t
     is range -(2 ** (SIZEOF_tv_sec - 1)) .. 2 ** (SIZEOF_tv_sec - 1) - 1;
   for timeval_tv_sec_t'Size use SIZEOF_tv_sec;

   type timeval_tv_usec_t
     is range -(2 ** (SIZEOF_tv_usec - 1)) .. 2 ** (SIZEOF_tv_usec - 1) - 1;
   for timeval_tv_usec_t'Size use SIZEOF_tv_usec;

   type Timeval is record
      tv_sec  : timeval_tv_sec_t;  -- Seconds
      tv_usec : timeval_tv_usec_t; -- Microseconds
   end record;
   pragma Convention (C, Timeval);

   type sa_family_t is mod 2 ** SIZEOF_sin_family;
   for sa_family_t'Size use SIZEOF_sin_family;
@IF AI_FAMILY_OFFSET > 0

   type In6_Addr is array (1 .. 8) of Interfaces.Unsigned_16;
   pragma Convention (C, In6_Addr);

   type Sockaddr_In6 is record
@IF SIN_FAMILY_OFFSET = 1
      Length   : Interfaces.Unsigned_8 := 0;
@ELSE
@IF SIN_FAMILY_OFFSET /= 0
      Unexpected sockaddr_in6 field family offset.
@END_IF
@END_IF
      Family   : sa_family_t := 0;
      Port     : Interfaces.C.unsigned_short := 0;
      FlowInfo : Interfaces.Unsigned_32 := 0;
      Addr     : In6_Addr := (others => 0);
      Scope_Id : Interfaces.Unsigned_32 := 0;
   end record;
   pragma Convention (C, Sockaddr_In6);
@END_IF

   type Addr_Info;
   type Addr_Info_Access is access all Addr_Info;

   type Addr_Info is record
      ai_flags     : C.int;
      ai_family    : C.int;
      ai_socktype  : C.int;
      ai_protocol  : C.int;
      ai_addrlen   : socklen_t;
@IF AI_CANONNAME_OFFSET < AI_ADDR_OFFSET
      ai_canonname : C.Strings.chars_ptr;
      ai_addr      : System.Address;
@ELSE
      ai_addr      : System.Address;
      ai_canonname : C.Strings.chars_ptr;
@END_IF
      ai_next      : Addr_Info_Access;
   end record;
   pragma Convention (C, Addr_Info);
*/

/*

   --  Some routines

   function GetAddrInfo
     (node    : C.Strings.chars_ptr;
      service : C.Strings.chars_ptr;
      hints   : Addr_Info;
      res     : not null access Addr_Info_Access) return C.int;

   procedure FreeAddrInfo (res : Addr_Info_Access);

   function GAI_StrError (ecode : C.int) return C.Strings.chars_ptr;

   function Socket_StrError (ecode : Integer) return C.Strings.chars_ptr;

   function Set_Sock_Opt
     (S       : C.int;
      Level   : C.int;
      OptName : C.int;
      OptVal  : System.Address;
      OptLen  : C.int) return C.int;

   function C_Ioctl (S : C.int; Req : C.int; Arg : access C.int) return C.int;

   function C_Close (Fd : C.int) return C.int;
*/

#ifdef _WIN32
/*

   pragma Linker_Options ("-lws2_32");

   procedure WSA_Startup (Version : C.int; Data : System.Address);
   pragma Import (Stdcall, WSA_Startup, "WSAStartup");

   function Socket_Errno return Integer;
*/
#else
/*
   procedure WSA_Startup (Version : C.int; Data : System.Address) is null;
   function Socket_Errno return Integer renames GNAT.OS_Lib.Errno;
*/
#endif

#if defined (__MINGW32__)
/*

   ------------------------------
   -- MinGW-specific constants --
   ------------------------------

*/

CND(WSASYSNOTREADY,     "System not ready")
CND(WSAVERNOTSUPPORTED, "Version not supported")
CND(WSANOTINITIALISED,  "Winsock not initialized")
CND(WSAEDISCON,         "Disconnected")

#endif /* __MINGW32__ */

/**
 ** End of constants definitions
 **/

/*

private

*/

#ifdef _WIN32
/*
   pragma Import (Stdcall, GetAddrInfo, "getaddrinfo");
   pragma Import (Stdcall, FreeAddrInfo, "freeaddrinfo");
   pragma Import (Stdcall, Set_Sock_Opt, "setsockopt");

   pragma Import (C, GAI_StrError, "AWS_gai_strerror");
   pragma Import (C, Socket_StrError, "socket_strerror");
   pragma Import (Stdcall, C_Ioctl, "ioctlsocket");
   pragma Import (Stdcall, C_Close, "closesocket");
   pragma Import (Stdcall, Socket_Errno, "WSAGetLastError");
*/
#else
/*
   pragma Import (C, GetAddrInfo, "getaddrinfo");
   pragma Import (C, FreeAddrInfo, "freeaddrinfo");
   pragma Import (C, Set_Sock_Opt, "setsockopt");
   pragma Import (C, GAI_StrError, "gai_strerror");
   pragma Import (C, Socket_StrError, "strerror");
   pragma Import (C, C_Ioctl, "ioctl");
   pragma Import (C, C_Close, "close");
*/
#endif
/*

end AWS.OS_Lib;
*/
}
