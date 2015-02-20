/****************************************************************************
 *                             Ada Web Server                               *
 *                                                                          *
 *                      Copyright (C) 2015, AdaCore                         *
 *                                                                          *
 *  This is free software;  you can redistribute it  and/or modify it       *
 *  under terms of the  GNU General Public License as published  by the     *
 *  Free Software  Foundation;  either version 3,  or (at your option) any  *
 *  later version.  This software is distributed in the hope  that it will  *
 *  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty *
 *  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU     *
 *  General Public License for  more details.                               *
 *                                                                          *
 *  You should have  received  a copy of the GNU General  Public  License   *
 *  distributed  with  this  software;   see  file COPYING3.  If not, go    *
 *  to http://www.gnu.org/licenses for a complete copy of the license.      *
 ****************************************************************************/

#include <string.h>
#include <stdlib.h>

#include "Hello_USCOREBinding.nsmap"
#include "soapH.h"
#include "soapStub.h"

int main()
{
  struct soap soap;
  char *greeting = NULL;

  soap_init(&soap);

  if (soap_call___ns1__sayHello(&soap, "http://127.0.0.1:9111/hello",
                              "sayHello", "c_client", &greeting) == SOAP_OK)
    {
      printf ("%s\n", greeting);
    }
  else
    printf ("Error calling ns1__sayHello\n");

  soap_done(&soap); // close master socket and detach environment
}
