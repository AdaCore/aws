------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2002                            --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimkov - Pascal Obry                                --
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

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Command_Line;

with AWS.Jabber;

procedure Test_Jabber is

   use Ada;
   use Ada.Strings.Unbounded;

   procedure Parse_Command_Line;
   --  Parse the command line argument and set the variable below

   procedure Display_Usage;
   --  Ouput the usage information to the standard output

   Host   : Unbounded_String;
   Login  : Unbounded_String;
   Pwd    : Unbounded_String;
   User   : Unbounded_String;

   -------------------
   -- Display_Usage --
   -------------------

   procedure Display_Usage is
      use Text_IO;
   begin
      New_Line;
      Put_Line ("Usage : test_jabber -h host -l login -p pwd jid");
      New_Line;
      Put_Line ("  -h Host      Jabber host server");
      Put_Line ("  -l login     Login for the Jabber server account");
      Put_Line ("  -p password  Password for the Jabber server account");
      Put_Line ("  jid          The Jabber ID for which to check presence");
      New_Line;
   end Display_Usage;

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   procedure Parse_Command_Line is
   begin
      loop
         case GNAT.Command_Line.Getopt ("h: l: p:") is

            when ASCII.NUL =>
               exit;

            when 'h' =>
               Host := To_Unbounded_String (GNAT.Command_Line.Parameter);

            when 'l' =>
               Login := To_Unbounded_String (GNAT.Command_Line.Parameter);

            when 'p' =>
               Pwd := To_Unbounded_String (GNAT.Command_Line.Parameter);

            when others =>
               raise Program_Error;         -- cannot occurs!
         end case;
      end loop;

      User := To_Unbounded_String (GNAT.Command_Line.Get_Argument);
   end Parse_Command_Line;

   Server : AWS.Jabber.Server;
   Status : AWS.Jabber.Presence_Status;
begin
   Parse_Command_Line;

   if Host = Null_Unbounded_String
     or else Login = Null_Unbounded_String
     or else Pwd = Null_Unbounded_String
     or else User = Null_Unbounded_String
   then
      Display_Usage;
      return;
   end if;

   AWS.Jabber.Connect
     (Server, To_String (Host), To_String (Login), To_String (Pwd));

   for K in 1 .. 2 loop
      AWS.Jabber.Check_Presence
        (Server, To_String (User) & '@' & To_String (Host), Status);

      Text_IO.Put_Line
        ("Status : " & AWS.Jabber.Presence_Status'Image (Status));
   end loop;

   AWS.Jabber.Close (Server);

exception
   when others =>
      Display_Usage;
end Test_Jabber;
