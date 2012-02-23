------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
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
with Ada.Exceptions;

with AWS.Client;
with AWS.Exceptions;
with AWS.MIME;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;
with AWS.Log;
with AWS.Utils;

procedure Unexph is

   use Ada;
   use Ada.Text_IO;
   use AWS;

   function CB (Request : Status.Data) return Response.Data;

   procedure UEH
     (E      : Ada.Exceptions.Exception_Occurrence;
      Log    : in out AWS.Log.Object;
      Error  : AWS.Exceptions.Data;
      Answer : in out Response.Data);

   HTTP : Server.HTTP;
   R    : Response.Data;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
   begin
      raise Program_Error;
      return Response.Build (MIME.Text_HTML, "be happy.");
   end CB;

   ---------
   -- UEH --
   ---------

   procedure UEH
     (E      : Ada.Exceptions.Exception_Occurrence;
      Log    : in out AWS.Log.Object;
      Error  : AWS.Exceptions.Data;
      Answer : in out Response.Data) is
   begin
      Ada.Text_IO.Put_Line
        (Ada.Exceptions.Exception_Name (E) & ' '
           & Boolean'Image (Error.Fatal));
   end UEH;

begin
   Server.Set_Unexpected_Exception_Handler (HTTP, UEH'Unrestricted_Access);

   Server.Start
     (HTTP, "Test unexpected exception handler", CB'Unrestricted_Access,
      Port => 0, Max_Connection => 3);

   R := Client.Get
          (AWS.Server.Status.Local_URL (HTTP) & "/test",
           Timeouts => Client.Timeouts
             (Connect => 1.0, Send => 2.0, Receive => 2.0));

   Server.Shutdown (HTTP);

exception
   when E : others =>
      Put_Line ("Main Error " & Ada.Exceptions.Exception_Information (E));
      Server.Shutdown (HTTP);
end Unexph;
