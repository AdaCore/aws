------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2002-2012, AdaCore                     --
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

with GNAT.Command_Line;

with AWS.Jabber.Client;

procedure Jabber_Demo is

   use Ada;
   use Ada.Strings.Unbounded;
   use AWS.Jabber.Client;

   Chat_Command : constant String := "/msg";

   procedure Chat
     (Account : AWS.Jabber.Client.Account; Command : String);
   --  Parse and execute the chat command

   procedure Parse_Command_Line;
   --  Parse the command line argument and set the variable below

   procedure Display_Usage;
   --  Ouput the usage information to the standard output

   Host   : Unbounded_String;
   Login  : Unbounded_String;
   Pwd    : Unbounded_String;

   ----------
   -- Chat --
   ----------

   procedure Chat
     (Account : AWS.Jabber.Client.Account;
      Command : String)
   is
      Sent : Boolean := False;
   begin
      Parse_Command : for K in Command'Range loop
         if Command (K) = ' ' then
            Send (Account => Account,
                  JID     => Jabber_ID (Command (Command'First .. K - 1)),
                  Content => Command (K + 1 .. Command'Last));
            Sent := True;
            exit Parse_Command;
         end if;
      end loop Parse_Command;

      if not Sent then
         --  Invalid line !
         Text_IO.Put_Line ("Can't parse line");
      end if;
   end Chat;

   -------------------
   -- Display_Usage --
   -------------------

   procedure Display_Usage is
      use Text_IO;
   begin
      New_Line;
      Put_Line ("Usage : jabber_demo -h host -l login -p pwd");
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
      Set_Login_Information (Account, To_String (Login), To_String (Pwd));
      Connect (Account);

      Text_IO.Put_Line ("Type /help for help");
      Main_Loop : loop
         Process_Line : declare
            Line : constant String := Text_IO.Get_Line;
         begin
            if Line (Line'First .. Line'First + Chat_Command'Length - 1)
              = Chat_Command
            then
               Chat
                 (Account,
                  Line (Line'First + Chat_Command'Length + 1 .. Line'Last));

            elsif Line = "/help" then
               Text_IO.Put_Line ("Type /msg user@server A chat message");
               Text_IO.Put_Line ("     to send a message to user@server");
               Text_IO.Put_Line ("Type /quit to quit");

            elsif Line = "/quit" then
               exit Main_Loop;

            else
               Text_IO.Put_Line ("Error can't parse line");
            end if;
         end Process_Line;
      end loop Main_Loop;

      Text_IO.Put_Line ("Closing");

      Close (Account);
   end Jabber_Session;

end Jabber_Demo;
