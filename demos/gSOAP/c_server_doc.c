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

int __ns1__sayHello(struct soap *soap, char * firstName, char **greeting)
{
  *greeting = (char*)malloc(100);
  strcpy(*greeting, "gSOAP: Hello ");
  strcat(*greeting, firstName);
  return 0;
}

int main()
{
  struct soap soap;
  int m, s; // master and slave sockets
  int i;

  soap_init(&soap);
  m = soap_bind(&soap, "127.0.0.1", 9111, 100);
  if (m < 0)
    soap_print_fault(&soap, stderr);
  else
    {
      fprintf(stderr, "Socket connection successful: master socket = %d\n", m);
      for (i = 1; ; i++)
        {
          s = soap_accept(&soap);
          if (s < 0)
            {
              soap_print_fault(&soap, stderr);
              break;
            }

          fprintf(stderr,
                  "%d: accepted connection from IP=%d.%d.%d.%d socket=%d\n", i,
                  (soap.ip >> 24)&0xFF, (soap.ip>>16)&0xFF, (soap.ip>>8)&0xFF,
                  soap.ip&0xFF, s);
          if(soap_serve(&soap) != SOAP_OK) // process RPC request
            soap_print_fault(&soap, stderr); // print error
          fprintf(stderr,"request served\n");
          soap_destroy(&soap); // clean up class instances
          soap_end(&soap); // clean up everything and close socket
        }
    }
  soap_done(&soap); // close master socket and detach environment
}
