//
//  Dynamic calls to protocol independent API functions if available, or
//  emulate it. This file would not be necessary after we would not support
//  Windows 2000 in AWS.
//
//  $Id$

#include <ws2tcpip.h>
#include "wspiapi.h"

int WSPIAPI_init ()
{
   //  This routine should be called in Ada elaboration process to initialize
   //  internal wspiapi.h static data and make subsequent calls thread safe.

   struct addrinfo Hints;
   struct addrinfo *Addr = NULL;
   int rc;

   memset(&Hints, 0, sizeof(Hints));
   Hints.ai_family   = PF_UNSPEC;
   Hints.ai_socktype = SOCK_STREAM;
   Hints.ai_flags    = AI_PASSIVE;

   rc = WspiapiGetAddrInfo (NULL, "5001", &Hints, &Addr);
   WspiapiFreeAddrInfo (Addr);

   return rc; 
}
