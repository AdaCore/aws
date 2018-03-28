------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                    Copyright (C) 2015-2018, AdaCore                      --
--                                                                          --
--  This is free software;  you can redistribute it  and/or modify it       --
--  under terms of the  GNU General Public License as published  by the     --
--  Free Software  Foundation;  either version 3,  or (at your option) any  --
--  later version.  This software is distributed in the hope  that it will  --
--  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU     --
--  General Public License for  more details.                               --
--                                                                          --
--  You should have  received  a copy of the GNU General  Public  License   --
--  distributed  with  this  software;   see  file COPYING3.  If not, go    --
--  to http://www.gnu.org/licenses for a complete copy of the license.      --
------------------------------------------------------------------------------

with AWS.MIME;

with SOAP.Utils;

with Hello_DOC.Server;

package body Hello_DOC_Server_CB is

   use AWS;

   function sayHello (Firstname : String) return String;

   -------------
   -- SOAP_CB --
   -------------

   function SOAP_CB is new Hello_DOC.Server.sayHello_CB (sayHello);

   function SOAP_Wrapper is new SOAP.Utils.SOAP_Wrapper (SOAP_CB);

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      SOAPAction : constant String := Status.SOAPAction (Request);
   begin
      if SOAPAction = "sayHello" then
         return SOAP_Wrapper (Request, Hello_DOC.Schema);
      else
         return Response.Build (MIME.Text_HTML, "<p>Not a SOAP request");
      end if;
   end CB;

   --------------
   -- sayHello --
   --------------

   function sayHello (Firstname : String) return String is
   begin
      return "Hello " & Firstname & " and welcome!";
   end sayHello;

end Hello_DOC_Server_CB;
