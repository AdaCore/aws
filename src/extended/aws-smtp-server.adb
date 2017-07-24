------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2008-2017, AdaCore                     --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with AWS.Headers;
with AWS.Net.Buffered;
with AWS.SMTP.Messages.Set;

package body AWS.SMTP.Server is

   use Ada;

   --  Messages definitions

   DATA : constant String := "DATA";
   HELO : constant String := "HELO";
   MAIL : constant String := "MAIL";
   QUIT : constant String := "QUIT";
   RCPT : constant String := "RCPT";

   function Get_Message (Sock : Net.Socket_Type'Class) return Messages.Data;
   --  Get an incoming message from the server

   ------------------
   -- Mail_Handler --
   ------------------

   task body Mail_Handler is
      Message : Messages.Data;
   begin
      accept Start;

      --  Server.Host.Secure could be used only after accept Start because
      --  task created before Server.Host initialized.

      declare
         Sock : Net.Socket_Type'Class :=
                  Net.Socket (Security => Server.Host.Secure);
      begin
         loop
            AWS.Net.Accept_Socket (Server.Host.Sock.all, Sock);

            Message := Get_Message (Sock);

            Server.Action (Message);

            Sock.Shutdown;
         end loop;
      end;

   exception
      when E : others =>
         if not Server.Shutdown then
            --  When server is shutdown we do not want to report errors. In
            --  some cases the socket layer won't end silently as the server
            --  socket is shutdown. See Shutdown routine below.
            Text_IO.Put_Line ("SMTP Server Fatal error");
            Text_IO.Put_Line (Exceptions.Exception_Information (E));
            raise;
         end if;
   end Mail_Handler;

   -----------------
   -- Get_Message --
   -----------------

   function Get_Message
     (Sock : Net.Socket_Type'Class) return Messages.Data
   is

      Empty_Line : constant String := "";
      CRLF       : constant String := ASCII.CR & ASCII.LF;

      procedure Read_Message_Body;
      --  Read message body from the current socket and set the Message_Body
      --  Unbounded String below. A message body is terminated by the sequence
      --  CRLF & '.' & CRLF.

      Message      : Messages.Data;
      Message_Body : Unbounded_String;
      Headers      : AWS.Headers.List;

      -----------------------
      -- Read_Message_Body --
      -----------------------

      procedure Read_Message_Body is
      begin
         Read_Message_Header : loop
            declare
               Line        : constant String := Net.Buffered.Get_Line (Sock);
               Split_Point : Natural;
            begin
               exit Read_Message_Header when Line = Empty_Line;
               Split_Point := Strings.Fixed.Index (Line, ":");

               Headers.Add
                 (Name  => Line (Line'First .. Split_Point - 1),
                  Value => Line (Split_Point + 2 .. Line'Last));
            end;
         end loop Read_Message_Header;

         --  https://tools.ietf.org/html/rfc5321#section-4.5.2

         Read_Message : loop
            declare
               Line  : constant String := Net.Buffered.Get_Line (Sock);
               First : Natural := Line'First;
            begin
               exit Read_Message when Line = ".";

               if Line'Length > 0 and then Line (Line'First) = '.' then
                  First := First + 1;
               end if;

               Append (Message_Body, Line (First .. Line'Last) & CRLF);
            end;
         end loop Read_Message;
      end Read_Message_Body;

   begin
      Net.Buffered.Put_Line (Sock, "220 AdaSC Service ready");
      --  ok, service is ready

      Read_Message : loop
         declare
            Line : constant String := Net.Buffered.Get_Line (Sock);
            Cmd  : constant String := Line (Line'First .. Line'First + 3);
         begin
            if Cmd = DATA then
               Net.Buffered.Put_Line (Sock, SMTP.Message (Start_Mail_Input));

               Read_Message_Body;
               Net.Buffered.Put_Line
                 (Sock, SMTP.Message (Requested_Action_Ok));

            elsif Cmd = HELO then
               Net.Buffered.Put_Line
                 (Sock, SMTP.Message (Requested_Action_Ok));

            elsif Cmd = MAIL then
               Net.Buffered.Put_Line
                 (Sock, SMTP.Message (Requested_Action_Ok));

            elsif Cmd = RCPT then
               Net.Buffered.Put_Line
                 (Sock, SMTP.Message (Requested_Action_Ok));

            elsif Cmd = QUIT then
               Net.Buffered.Put_Line (Sock, SMTP.Message (Service_Closing));
               exit Read_Message;

            else
               --  Command is not known

               Net.Buffered.Put_Line (Sock, SMTP.Message (Syntax_Error));

            end if;
         end;
      end loop Read_Message;

      Net.Buffered.Flush (Sock);

      Messages.Set.Set_Body (Message, To_String (Message_Body));
      Messages.Set.Headers (Message, Headers);

      return Message;
   end Get_Message;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown (Server : in out Handle) is
      Try : Natural := 5;
   begin
      --  Shutdown the socket, this should raise socket error on the
      --  Mail_Handler task.

      Server.Shutdown := True;

      Net.Shutdown (Server.Host.Sock.all);

      --  Now wait for the task to terminate

      while not Server.Server_Handler'Terminated and then Try > 0 loop
         delay 1.0;
         Try := Try - 1;
      end loop;

      --  If not yet terminated abort

      if not Server.Server_Handler'Terminated then
         abort Server.Server_Handler;
      end if;

      Net.Free (Server.Host.Sock);
   end Shutdown;

   -----------
   -- Start --
   -----------

   procedure Start
     (Server : in out Handle;
      Host   : Receiver;
      Action : Callback) is
   begin
      --  Initialize server listening port

      Server.Host := Host;

      Server.Host.Sock := Net.Socket (Security => False);

      Server.Host.Sock.Set_Timeout (Server.Host.Timeout);
      Server.Host.Sock.Bind (Port => Host.Port, Family => Host.Family);
      Server.Host.Sock.Listen;

      Server.Action := Action;

      Server.Server_Handler.Start;
   end Start;

end AWS.SMTP.Server;
