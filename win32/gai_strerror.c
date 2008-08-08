/////////////////////////////////////////////////////////////////////////////
//                             Ada Web Server                               /
//                                                                          /
//                    Copyright (C) 2003-2008, AdaCore                      /
//                                                                          /
//  This library is free software; you can redistribute it and/or modify    /
//  it under the terms of the GNU General Public License as published by    /
//  the Free Software Foundation; either version 2 of the License, or (at   /
//  your option) any later version.                                         /
//                                                                          /
//  This library is distributed in the hope that it will be useful, but     /
//  WITHOUT ANY WARRANTY; without even the implied warranty of              /
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       /
//  General Public License for more details.                                /
//                                                                          /
//  You should have received a copy of the GNU General Public License       /
//  along with this library; if not, write to the Free Software Foundation, /
//  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          /
//                                                                          /
//  As a special exception, if other files instantiate generics from this   /
//  unit, or you link this unit with other files to produce an executable,  /
//  this  unit  does not  by itself cause  the resulting executable to be   /
//  covered by the GNU General Public License. This exception does not      /
//  however invalidate any other reasons why the executable file  might be  /
//  covered by the  GNU Public License.                                     /
/////////////////////////////////////////////////////////////////////////////

//  Replacement of the not thread safe Win32 gai_strerror inlined in the
//  MSVC ws2tcpip.h. Note that ws2tcpip.h from Mingw (at least from GCC 3.4.1)
//  have only declaration of the gai_strerror.

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
    case EAI_NONAME:
      return "Neither nodename nor servname provided, or not known.";
    case EAI_SERVICE:
      return "The servname parameter is not supported for ai_socktype.";
    case EAI_SOCKTYPE:
      return "The ai_socktype member is not supported.";
    default:
      return "Unknown error.";
    }
}
