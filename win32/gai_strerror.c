//----------------------------------------------------------------------------
//  Replacement of the not thread safe Win32 gai_strerror inlined in the
//  MSVC ws2tcpip.h. Note that ws2tcpip.h from Mingw (at least from GCC 3.4.1)
//  have only declaration of the gai_strerror.
//
// $Id$

#include <ws2tcpip.h>

char * AWS_gai_strerror (int ecode)
{
   switch (ecode) {
   case EAI_AGAIN:    return "Temporary failure in name resolution.";
   case EAI_BADFLAGS: return "Invalid value for ai_flags.";
   case EAI_FAIL:     return "Nonrecoverable failure in name resolution.";
   case EAI_FAMILY:   return "The ai_family member is not supported.";
   case EAI_MEMORY:   return "Memory allocation failure.";
   case EAI_NODATA:   return "No address associated with nodename.";
   case EAI_NONAME:   return "Neither nodename nor servname provided, or not known.";
   case EAI_SERVICE:  return "The servname parameter is not supported for ai_socktype.";
   case EAI_SOCKTYPE: return "The ai_socktype member is not supported.";
   default:           return "Unknown error.";
   }
}
