------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2014, AdaCore                     --
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
with Ada.Exceptions;

with AWS.Server;
with AWS.Client;
with AWS.Communication.Client;
with AWS.Communication.Server;
with AWS.Status;
with AWS.MIME;
with AWS.Net;
with AWS.Response;
with AWS.Parameters;
with AWS.Messages;
with AWS.Utils;

with Get_Free_Port;

procedure Tcom is

   use Ada;
   use Ada.Strings.Unbounded;
   use Ada.Text_IO;
   use AWS;

   type Context is record
      X : Natural := 0;
   end record;

   type Context_Access is access all Context;

   Free_Port : Positive := 1254;

   function Callback
     (Server     : String;
      Name       : String;
      Context    : not null access Tcom.Context;
      Parameters : Communication.Parameter_Set
        := Communication.Null_Parameter_Set)
      return Response.Data;

   function "+"
     (Str : String)
      return Unbounded_String
      renames To_Unbounded_String;

   --------------
   -- Callback --
   --------------

   function Callback
     (Server     : String;
      Name       : String;
      Context    : not null access Tcom.Context;
      Parameters : Communication.Parameter_Set
        := Communication.Null_Parameter_Set)
      return Response.Data
   is
      X : constant String := Utils.Image (Context.X);
   begin
      if Server = "" then
         Put_Line ("NOK, Server is not set");
      else
         Put_Line ("Ok, Server is set");
      end if;

      Put_Line ("         " & Name);
      Put_Line ("         " & X);

      for K in Parameters'Range loop
         Put_Line ("    " & Positive'Image (K)
                     & " = " & To_String (Parameters (K)));
      end loop;

      Context.X := Context.X + 1;

      return Response.Build (MIME.Text_HTML, "Ok, " & X);
   end Callback;

   package Com_Server is
      new AWS.Communication.Server (Context, Context_Access, Callback);

   C : aliased Context;

   R : Response.Data;

   LH : constant String := Net.Localhost (Net.IPv6_Available);

begin
   Get_Free_Port (Free_Port);

   Com_Server.Start
     (Context => C'Access, Host => LH, Port => Free_Port);

   R := Communication.Client.Send_Message (LH, Free_Port, "zero");

   Put_Line ("R1 : " & Response.Message_Body (R));
   New_Line;

   R := Communication.Client.Send_Message
     (LH, Free_Port, "one", (1 => +"first"));

   Put_Line ("R2 : " & Response.Message_Body (R));
   New_Line;

   R := Communication.Client.Send_Message
     (LH, Free_Port, "two", (+"first", +"second"));

   Put_Line ("R3 : " & Response.Message_Body (R));
   New_Line;

   Com_Server.Shutdown;
exception
   when E : others =>
      Put_Line ("Main Error " & Ada.Exceptions.Exception_Information (E));
end Tcom;
