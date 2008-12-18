------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2008, AdaCore                     --
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

--  Test hotplug feature

with Ada.Exceptions;
with Ada.Text_IO;

with AWS.Client.Hotplug;
with AWS.Messages;
with AWS.Response;
with AWS.Server.Hotplug;
with AWS.Utils;

with SOAP_Hotplug_Pack;
with SOAP_Hotplug_CB;
with SOAP_Hotplug_Pack_Service.Client;
with Get_Free_Port;

procedure Test_SOAP_Hotplug is

   use Ada;
   use Ada.Exceptions;
   use AWS;

   Filter : constant String := "/jo.*";

   task Hotplug_Server is
      entry Start;
      entry Started;
      entry Stop;
   end Hotplug_Server;

   procedure Request (X, Y : in Integer);
   --  Request URI resource to main server, output result

   Hotplug_Port : Natural := 1135;
   Main_Port    : Natural := 1136;
   Com_Port     : Natural := 2122;

   --------------------
   -- Hotplug_Server --
   --------------------

   task body Hotplug_Server is
      use type Messages.Status_Code;
      WS : Server.HTTP;
      R  : Response.Data;
   begin
      accept  Start;

      Server.Start
        (WS, "Hotplug",
         Admin_URI      => "/Admin-Page",
         Port           => Hotplug_Port,
         Max_Connection => 3,
         Callback       => SOAP_Hotplug_CB.Hotplug'Access);

      R := Client.Hotplug.Register
        ("hp_test", SOAP_Hotplug_CB.Password,
         "http://localhost:" & Utils.Image (Com_Port),
         Filter, "http://localhost:" & Utils.Image (Hotplug_Port));

      if Response.Status_Code (R) = Messages.S200 then
         Text_IO.Put_Line ("Register OK");
      else
         Text_IO.Put_Line
           ("Register Error : " & Response.Message_Body (R));
         raise Constraint_Error;
      end if;
      Text_IO.Flush;

      accept  Started;

      accept Stop do
         R := AWS.Client.Hotplug.Unregister
           ("hp_test", SOAP_Hotplug_CB.Password,
            "http://localhost:" & Utils.Image (Com_Port), Filter);

         if Response.Status_Code (R) = Messages.S200 then
            Text_IO.Put_Line ("Unregister OK");
         else
            Text_IO.Put_Line
              ("Unregister Error : " & Response.Message_Body (R));
         end if;
         Text_IO.Flush;

         Server.Shutdown (WS);
      end Stop;
   exception
      when others =>
         Server.Shutdown (WS);
   end Hotplug_Server;

   -------------
   -- Request --
   -------------

   procedure Request (X, Y : in Integer) is
      R : Integer;
   begin
      R := SOAP_Hotplug_Pack_Service.Client.Job1
        (X, Y, "http://localhost:" & Utils.Image (Main_Port) & "/job");
      Text_IO.Put_Line ("Response: " & Utils.Image (R));
      R := SOAP_Hotplug_Pack_Service.Client.Job2
        (X, Y, "http://localhost:" & Utils.Image (Main_Port) & "/job");
      Text_IO.Put_Line ("Response: " & Utils.Image (R));
      Text_IO.Flush;
   end Request;

begin
   Text_IO.Put_Line ("Starting main server...");

   Get_Free_Port (Hotplug_Port);
   Get_Free_Port (Main_Port);
   Get_Free_Port (Com_Port);

   --  Write access file

   declare
      F : Text_IO.File_Type;
   begin
      Text_IO.Create (F, Text_IO.Out_File, "hotplug_access.ini");
      Text_IO.Put_Line
        (F, "hp_test:f8de61f1f97df3613fbe29b031eb52c6:localhost:"
         & Utils.Image (Hotplug_Port));
      Text_IO.Close (F);
   end;

   Server.Start
     (SOAP_Hotplug_CB.Main_Server, "Main",
      Admin_URI      => "/Admin-Page",
      Port           => Main_Port,
      Max_Connection => 3,
      Callback       => SOAP_Hotplug_CB.Main'Access);

   Server.Hotplug.Activate
     (SOAP_Hotplug_CB.Main_Server'Access, Com_Port, "hotplug_access.ini");

   --  Send some requests

   Request (3, 7);
   Request (9, 2);

   --  Start hotplug now

   Hotplug_Server.Start;
   Hotplug_Server.Started;

   Request (3, 7);
   Request (9, 2);

   Text_IO.Put_Line ("Stop hotplug server");
   Text_IO.Flush;

   Hotplug_Server.Stop;

   Text_IO.Put_Line ("Shutdown hotplug server support");
   Text_IO.Flush;

   AWS.Server.Hotplug.Shutdown;

   Text_IO.Put_Line ("Shutdown main server");
   Text_IO.Flush;

   Server.Shutdown (SOAP_Hotplug_CB.Main_Server);

exception
   when E : others =>
      Text_IO.Put_Line ("Exception raised:" & Exception_Information (E));
      Text_IO.Flush;
      Hotplug_Server.Stop;
      AWS.Server.Hotplug.Shutdown;
      Server.Shutdown (SOAP_Hotplug_CB.Main_Server);
end Test_SOAP_Hotplug;
