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

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.MIME;

with SOAP.Message;
with SOAP.Types;
with SOAP.Utils;

with testingservice.Server;
with testingservice.Types;

package body Dl2_Server_CB is

   use Ada;
   use Ada.Strings.Unbounded;
   use AWS;

   Count : Natural := 0;

   use testingservice.Server;
   use testingservice.Types;

   function getQueueStatus
     (getQueueStatusRequest : GetQueueStatusRequest_Type)
     return getQueueStatus_Result;

   function execute
     (executionTime : SOAP.Types.Local_Time;
      valueA        : RecB_Set_Type)
     return String;

   -------------
   -- SOAP_CB --
   -------------

   function SOAP_CB is
     new testingservice.Server.getQueueStatus_CB (getQueueStatus);

   function SOAP_CB_E is new testingservice.Server.execute_CB (execute);

   function SOAP_Wrapper   is new SOAP.Utils.SOAP_Wrapper (SOAP_CB);
   function SOAP_Wrapper_E is new SOAP.Utils.SOAP_Wrapper (SOAP_CB_E);

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      SOAPAction : constant String := Status.SOAPAction (Request);
      Payload    : constant String := Status.Payload (Request);
   begin
      Text_IO.Put_Line (Payload);

      if SOAPAction = "http://aurn.here.org/abc/execute" then
         return SOAP_Wrapper_E (Request, testingservice.Schema);
      else
         return SOAP_Wrapper (Request, testingservice.Schema);
      end if;
   end CB;

   -------------
   -- execute --
   -------------

   function execute
     (executionTime : SOAP.Types.Local_Time;
      valueA        : RecB_Set_Type)
     return String is
   begin
      Count := Count + 1;
      return ("this_is_the_token" & Natural'Image (Count));
   end execute;

   --------------------
   -- getQueueStatus --
   --------------------

   function getQueueStatus
     (getQueueStatusRequest : GetQueueStatusRequest_Type)
     return getQueueStatus_Result
   is
      R : getQueueStatus_Result;
   begin
      R.numberQueued := 1;
      R.maximumQueueSize := 16;
      return R;
   end Getqueuestatus;

end Dl2_Server_CB;
