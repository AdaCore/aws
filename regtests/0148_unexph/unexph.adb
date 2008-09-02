------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2008, AdaCore                     --
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

with Ada.Text_IO;
with Ada.Exceptions;

with AWS.Client;
with AWS.Exceptions;
with AWS.MIME;
with AWS.Response;
with AWS.Server;
with AWS.Status;
with AWS.Log;
with AWS.Utils;

with Get_Free_Port;

procedure Unexph is

   use Ada;
   use Ada.Text_IO;
   use AWS;

   function CB (Request : in Status.Data) return Response.Data;

   procedure UEH
     (E      : in     Ada.Exceptions.Exception_Occurrence;
      Log    : in out AWS.Log.Object;
      Error  : in     AWS.Exceptions.Data;
      Answer : in out Response.Data);

   task Server is
      entry Wait_Start;
      entry Stop;
   end Server;

   HTTP : AWS.Server.HTTP;
   Port : Natural := 1240;

   R : Response.Data;

   --------
   -- CB --
   --------

   function CB (Request : in Status.Data) return Response.Data is
   begin
      raise Program_Error;
      return Response.Build (MIME.Text_HTML, "be happy.");
   end CB;

   ------------
   -- Server --
   ------------

   task body Server is
   begin
      AWS.Server.Set_Unexpected_Exception_Handler
        (HTTP, UEH'Unrestricted_Access);

      Get_Free_Port (Port);

      AWS.Server.Start
        (HTTP, "Test unexpected exception handler",
         CB'Unrestricted_Access, Port => Port, Max_Connection => 3);

      accept Wait_Start;
      accept Stop;

   exception
      when E : others =>
         Put_Line ("Server Error " & Ada.Exceptions.Exception_Information (E));
   end Server;

   ---------
   -- UEH --
   ---------

   procedure UEH
     (E      : in     Ada.Exceptions.Exception_Occurrence;
      Log    : in out AWS.Log.Object;
      Error  : in     AWS.Exceptions.Data;
      Answer : in out Response.Data) is
   begin
      Ada.Text_IO.Put_Line
        (Ada.Exceptions.Exception_Name (E) & ' '
           & Boolean'Image (Error.Fatal));
   end UEH;

begin
   Server.Wait_Start;

   R := Client.Get
          ("http://localhost:" & Utils.Image (Port) & "/test",
           Timeouts => (1.0, 2.0, 2.0, others => <>));

   Server.Stop;

exception
   when E : others =>
      Server.Stop;
      Put_Line ("Main Error " & Ada.Exceptions.Exception_Information (E));
end Unexph;
