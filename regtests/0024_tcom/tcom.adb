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

with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Exceptions;

with AWS.Server;
with AWS.Client;
with AWS.Communication.Client;
with AWS.Communication.Server;
with AWS.Status;
with AWS.MIME;
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
     (Server     : in String;
      Name       : in String;
      Context    : in Context_Access;
      Parameters : in Communication.Parameter_Set
        := Communication.Null_Parameter_Set)
      return Response.Data;

   function "+"
     (Str : in String)
      return Unbounded_String
      renames To_Unbounded_String;

   --------------
   -- Callback --
   --------------

   function Callback
     (Server     : in String;
      Name       : in String;
      Context    : in Context_Access;
      Parameters : in Communication.Parameter_Set
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

begin
   Get_Free_Port (Free_Port);

   Com_Server.Start (Free_Port, C'Access);

   R := Communication.Client.Send_Message
     ("localhost", Free_Port, "zero");

   Put_Line ("R1 : " & Response.Message_Body (R));
   New_Line;

   R := Communication.Client.Send_Message
     ("localhost", Free_Port, "one", (1 => +"first"));

   Put_Line ("R2 : " & Response.Message_Body (R));
   New_Line;

   R := Communication.Client.Send_Message
     ("localhost", Free_Port, "two", (+"first", +"second"));

   Put_Line ("R3 : " & Response.Message_Body (R));
   New_Line;

   Com_Server.Shutdown;
exception
   when E : others =>
      Put_Line ("Main Error " & Ada.Exceptions.Exception_Information (E));
end Tcom;
