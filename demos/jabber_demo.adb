------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                   Copyright (C) 2002-2008, AdaCore                       --
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

with GNAT.Command_Line;

with AWS.Jabber.Client;

procedure Jabber_Demo is

   use Ada;
   use Ada.Strings.Unbounded;
   use AWS.Jabber.Client;

   procedure Parse_Command_Line;
   --  Parse the command line argument and set the variable below

   procedure Display_Usage;
   --  Ouput the usage information to the standard output

   Host   : Unbounded_String;
   Login  : Unbounded_String;
   Pwd    : Unbounded_String;

   -------------------
   -- Display_Usage --
   -------------------

   procedure Display_Usage is
      use Text_IO;
   begin
      New_Line;
      Put_Line ("Usage : test_jabber -h host -l login -p pwd");
      New_Line;
      Put_Line ("  -h Host      Jabber host server");
      Put_Line ("  -l login     Login for the Jabber server account");
      Put_Line ("  -p password  Password for the Jabber server account");
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

   exception
      when others =>
         Display_Usage;
   end Parse_Command_Line;

--   Status : AWS.Jabber.Presence_Status;
begin
   Parse_Command_Line;

   if Host = Null_Unbounded_String
     or else Login = Null_Unbounded_String
     or else Pwd = Null_Unbounded_String
   then
      Display_Usage;
      return;
   end if;

   Jabber_Session : declare
      Account : AWS.Jabber.Client.Account;

   begin
      Set_Host (Account, To_String (Host));
      Set_Login_Information (Account,
                             To_String (Login),
                             To_String (Pwd));
      Connect (Account);

      Text_IO.Put_Line ("Type /help for help");
      loop
         Process_Line : declare
            Line : constant String := Text_IO.Get_Line;
         begin
            exit when Line = "/quit";
         end Process_Line;
      end loop;

      Text_IO.Put_Line ("Closing");

      Close (Account);
   end Jabber_Session;

end Jabber_Demo;
