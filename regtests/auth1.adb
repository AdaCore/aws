------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2004                          --
--                               ACT-Europe                                 --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

--  $Id$

--  ~ MAIN [STD]

with Ada.Text_IO;
with Ada.Exceptions;

with AWS.Client;
with AWS.Digest;
with AWS.Server;
with AWS.Status;
with AWS.MIME;
with AWS.Response;
with AWS.Messages;
with AWS.Utils;

with Get_Free_Port;

procedure Auth1 is

   use Ada;
   use Ada.Text_IO;
   use AWS;

   function CB (Request : in Status.Data) return Response.Data;

   task Server is
      entry Wait_Start;
      entry Stop;
   end Server;

   HTTP : AWS.Server.HTTP;

   Connect : Client.HTTP_Connection;

   Basic_Protected_URI  : constant String := "/Basic";
   Digest_Protected_URI : constant String := "/Digest";
   Any_Protected_URI    : constant String := "/Any";

   Auth_Username : constant String := "AWS";
   Auth_Password : constant String := "letmein";

   R    : Response.Data;
   Port : Natural := 1236;

   --------
   -- CB --
   --------

   function CB (Request : in Status.Data) return Response.Data is
      Username    : String := AWS.Status.Authorization_Name (Request);
      Valid_Nonce : Boolean;
   begin
      if Status.URI (Request) = Basic_Protected_URI then

         if Username = Auth_Username
           and then AWS.Status.Authorization_Password (Request) = Auth_Password
         then
            return AWS.Response.Build
              ("text/plain", "Basic authorization OK!");
         else
            return AWS.Response.Authenticate
              ("AWS regtest", AWS.Response.Basic);
         end if;

      elsif Status.URI (Request) = Digest_Protected_URI then
         Valid_Nonce
           := Digest.Check_Nonce (Status.Authorization_Nonce (Request));

         if AWS.Status.Check_Digest (Request, Auth_Password)
           and then Valid_Nonce
         then
            return AWS.Response.Build
              ("text/plain", "Digest authorization OK!");
         else
            return AWS.Response.Authenticate
              ("AWS regtest", AWS.Response.Digest, Stale => not Valid_Nonce);
         end if;

      elsif Status.URI (Request) = Any_Protected_URI then
         Valid_Nonce
           := Digest.Check_Nonce (Status.Authorization_Nonce (Request));

         if AWS.Status.Check_Digest (Request, Auth_Password)
           and then Valid_Nonce
         then
            return AWS.Response.Build
              ("text/plain", "AWS strongest authorization OK!");
         else
            return AWS.Response.Authenticate
              ("AWS regtest", AWS.Response.Any, Stale => not Valid_Nonce);
         end if;

      else
         return AWS.Response.Build
           ("text/plain", "No authentication.");
      end if;
   end CB;

   ------------
   -- Server --
   ------------

   task body Server is
   begin
      Get_Free_Port (Port);

      AWS.Server.Start
        (HTTP, "Test authentication.",
         CB'Unrestricted_Access, Port => Port, Max_Connection => 3);

      accept Wait_Start;
      accept Stop;

   exception
      when E : others =>
         Put_Line ("Server Error " & Exceptions.Exception_Information (E));
   end Server;

begin
   Server.Wait_Start;

   Client.Create
     (Connection => Connect,
      Host       => "http://localhost:" & Utils.Image (Port),
      Timeouts   => (5.0, 5.0));

   --  Test for basic authentication.

   Client.Set_WWW_Authentication
     (Connect, Auth_Username, "Wrong_Password", Client.Basic);

   Client.Get (Connect, R, "/Basic?param=value");
   Put_Line ("-> " & Messages.Image (Response.Status_Code (R)));

   Client.Set_WWW_Authentication
     (Connect, Auth_Username, Auth_Password, Client.Basic);

   Client.Get (Connect, R, "/Basic?param=value");
   Put_Line ("-> " & Messages.Image (Response.Status_Code (R)));
   Put_Line ("-> " & Response.Message_Body (R));

   --  Test for digest authentication.

   Client.Set_WWW_Authentication
     (Connect, Auth_Username, "Wrong_Password", Client.Digest);

   Client.Get (Connect, R, "/Digest?param=value");
   Put_Line ("-> " & Messages.Image (Response.Status_Code (R)));

   Client.Set_WWW_Authentication
     (Connect, Auth_Username, Auth_Password, Client.Digest);

   Client.Get (Connect, R, "/Digest?param=value");
   Put_Line ("-> " & Messages.Image (Response.Status_Code (R)));
   Put_Line ("-> " & Response.Message_Body (R));

   --  Test for strongest authentication.

   Client.Set_WWW_Authentication
     (Connect, Auth_Username, "Wrong_Password", Client.Any);

   Client.Get (Connect, R, "/Any?param=value");
   Put_Line ("-> " & Messages.Image (Response.Status_Code (R)));

   Client.Set_WWW_Authentication
     (Connect, Auth_Username, Auth_Password, Client.Any);

   Client.Get (Connect, R, "/Any?param=value");
   Put_Line ("-> " & Messages.Image (Response.Status_Code (R)));
   Put_Line ("-> " & Response.Message_Body (R));

   Client.Close (Connect);

   Server.Stop;

exception
   when E : others =>
      Server.Stop;
      Put_Line ("Main Error " & Exceptions.Exception_Information (E));
end Auth1;
