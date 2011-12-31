------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2012, AdaCore                     --
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

with SOAP_Hotplug_Pack_Service.CB;
with SOAP_Hotplug_Pack_Service.Server;

package body SOAP_Hotplug_CB is

   function Hotplug_Job (X, Y : Integer) return Integer;

   function Hotplug_Job (X, Y : Integer) return Integer is
   begin
      return X * Y;
   end Hotplug_Job;

   function SOAP_Wrapper is
     new SOAP.Utils.SOAP_Wrapper (SOAP_Hotplug_Pack_Service.CB.SOAP_CB);

   function SOAP_Hotplug_Wrapper is new SOAP.Utils.SOAP_Wrapper
     (SOAP_Hotplug_Pack_Service.CB.SOAP_Hotplug_CB);

   -------------
   -- Hotplug --
   -------------

   function Hotplug (Request : Status.Data) return Response.Data is
      SOAPAction : constant String := Status.SOAPAction (Request);
   begin
      if SOAPAction = "Job1"
        or else SOAPAction = "Job2"
      then
         return SOAP_Hotplug_Wrapper (Request);
      else
         return Response.Build (MIME.Text_HTML, "<p>Not a valid SOAP request");
      end if;
   end Hotplug;

   ----------
   -- Main --
   ----------

   function Main (Request : Status.Data) return Response.Data is
      SOAPAction : constant String := Status.SOAPAction (Request);
   begin
      if SOAPAction = "Job1"
        or else SOAPAction = "Job2"
      then
         return SOAP_Wrapper (Request);
      else
         return Response.Build (MIME.Text_HTML, "<p>Not a valid SOAP request");
      end if;
   end Main;

end SOAP_hotplug_CB;
