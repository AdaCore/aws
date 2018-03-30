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

with Ada.Text_IO;

with AWS.Messages;
with AWS.MIME;

with SOAP.Utils;

with Simpledocservice.Server;
with Simpledocservice.Types;

package body SD_Server_CB is

   use Ada;
   use AWS;
   use Simpledocservice.Server;
   use Simpledocservice.Types;

   function Call (X : Integer; Y : Float) return call_Result;

   -------------
   -- SOAP_CB --
   -------------

   function SOAP_CB is new Simpledocservice.Server.call_CB (Call);

   function SOAP_Wrapper is new SOAP.Utils.SOAP_Wrapper (SOAP_CB);

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
   begin
      return Response.Build (MIME.Text_HTML, "not used");
   end CB;

   ----------
   -- S_CB --
   ----------

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
               SOAP_Wrapper (Request, Simpledocservice.Schema);
      begin
         Text_IO.Put_Line ("=============== rep");
         Text_IO.Put_Line (Response.Message_Body (R));
         return R;
      end;
   end S_CB;

   --------------------
   -- getQueueStatus --
   --------------------

   function Call (X : Integer; Y : Float) return call_Result is
      R : call_Result;
   begin
      return R;
   end Call;

end SD_Server_CB;
