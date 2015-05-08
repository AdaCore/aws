------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2015, AdaCore                        --
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

with Ada.Text_IO;

with AWS.Messages;
with AWS.MIME;

with SOAP.Utils;

with Getcasualtiesservice.Server;
with Getcasualtiesservice.Types;

package body Server_CB is

   use Ada;
   use AWS;
   use Getcasualtiesservice.Server;
   use Getcasualtiesservice.Types;

   function getQueueStatus
     (getQueueStatusRequest : GetQueueStatusRequest_Type)
     return getQueueStatus_Result;

   -------------
   -- SOAP_CB --
   -------------

   function SOAP_CB is
     new Getcasualtiesservice.Server.getQueueStatus_CB (getQueueStatus);

   function SOAP_Wrapper is new SOAP.Utils.SOAP_Wrapper (SOAP_CB);

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
   begin
      return Response.Build (MIME.Text_HTML, "not used");
   end CB;

   function S_CB
     (SOAPAction : String;
      Payload    : SOAP.Message.Payload.Object;
      Request    : AWS.Status.Data)
      return Response.Data is
   begin
      Text_IO.Put_Line ("=============== req");
      Text_IO.Put_Line (Status.Payload (Request));

      declare
         R : constant Response.Data :=
               SOAP_Wrapper (Request, Getcasualtiesservice.Schema);
      begin
         Text_IO.Put_Line ("=============== rep");
         Text_IO.Put_Line (Response.Message_Body (R));
         return R;
      end;
   end S_CB;

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

end Server_CB;
