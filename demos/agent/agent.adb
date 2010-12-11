------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2010, AdaCore                     --
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

--  Usage: agent [options] [GET/PUT] <URL>
--         -f                      force display of message body.
--         -o                      output result in file agent.out.
--         -k                      keep-alive connection.
--         -s                      server-push mode.
--         -r                      follow redirection.
--         -i                      repeat over delay interval for stress test.
--         -d                      debug mode, view HTTP headers.
--         -c                      display server certificate.
--         -sc                     Set cookie.
--         -cc                     client certificate
--         -t                      wait response timeout.
--         -proxy <proxy_url>
--         -u <user_name>
--         -p <password>
--         -a <www_authentication_mode (Any, Basic or Digest)>
--         -pu <proxy_user_name>
--         -pp <proxy_password>
--         -pa <proxy_authentication_mode (Any, Basic or Digest)>
--
--  for example:
--
--     agent GET http://perso.wanadoo.fr/pascal.obry/contrib.html
--

with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Command_Line;
with Ada.Characters.Handling;
with Ada.Streams.Stream_IO;
with Ada.Exceptions;

with GNAT.Command_Line;

with AWS.Client;
with AWS.Default;
with AWS.Resources;
with AWS.Response;
with AWS.Messages;
with AWS.MIME;
with AWS.Net.SSL.Certificate;
with AWS.Status;
with AWS.URL;

procedure Agent is

   use AWS;
   use Ada;
   use Ada.Strings.Unbounded;
   use type Status.Request_Method;
   use type Messages.Status_Code;
   use type Response.Content_Length_Type;

   Syntax_Error : exception;

   Method             : Status.Request_Method;
   User               : Unbounded_String;
   Pwd                : Unbounded_String;
   WWW_Auth           : Client.Authentication_Mode := Client.Basic;
   URL                : Unbounded_String;
   Proxy              : Unbounded_String;
   Proxy_User         : Unbounded_String;
   Proxy_Pwd          : Unbounded_String;
   Proxy_Auth         : Client.Authentication_Mode := Client.Basic;
   Force              : Boolean := False;
   File               : Boolean := False;
   Keep_Alive         : Boolean := False;
   Server_Push        : Boolean := False;
   Follow_Redirection : Boolean := False;
   Show_Cert          : Boolean := False;
   Interval           : Duration               := Duration'Last;
   Timeouts           : Client.Timeouts_Values := Client.No_Timeout;
   Connect            : AWS.Client.HTTP_Connection;
   Cookie             : Unbounded_String;
   Client_Cert        : Unbounded_String
     := To_Unbounded_String (Default.Client_Certificate);

   function Parse_Command_Line return Boolean;
   --  Parse Agent command line. Returns False on error

   function Get_Auth_Mode (Mode : String) return Client.Authentication_Mode;
   --  Return the authentication value from the string representation.
   --  raises the human readable exception on error.

   procedure Show_Certificate (Cert : Net.SSL.Certificate.Object);

   procedure Usage;

   -------------------
   -- Get_Auth_Mode --
   -------------------

   function Get_Auth_Mode
     (Mode : String) return Client.Authentication_Mode is
   begin
      return Client.Authentication_Mode'Value (Mode);
   exception
      when Constraint_Error =>
         Ada.Exceptions.Raise_Exception
           (Constraint_Error'Identity,
            "Authentication mode should be ""Basic"", ""Digest"" or ""Any"".");
   end Get_Auth_Mode;

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   function Parse_Command_Line return Boolean is
   begin
      loop
         case GNAT.Command_Line.Getopt
           ("f o d h u: p: a: pu: pp: pa: proxy: k i: s sc: r c cc: t:")
         is
            when ASCII.NUL =>
               exit;

            when 'h' =>
               Usage;
               return False;

            when 'f' =>
               Force := True;

            when 'o' =>
               File := True;

            when 'i' =>
               Interval := Duration'Value (GNAT.Command_Line.Parameter);

            when 'd' =>
               AWS.Client.Set_Debug (On => True);

            when 'k' =>
               Keep_Alive := True;

            when 'r' =>
               Follow_Redirection := True;

            when 's' =>
               if GNAT.Command_Line.Full_Switch = "sc" then
                  Cookie := To_Unbounded_String (GNAT.Command_Line.Parameter);
               else
                  Server_Push := True;
               end if;

            when 'c' =>
               if GNAT.Command_Line.Full_Switch = "cc" then
                  Client_Cert
                    := To_Unbounded_String (GNAT.Command_Line.Parameter);
               else
                  Show_Cert := True;
               end if;

            when 'u' =>
               User := To_Unbounded_String (GNAT.Command_Line.Parameter);

            when 'a' =>
               WWW_Auth :=
                  Get_Auth_Mode (GNAT.Command_Line.Parameter);

            when 't' =>
               Timeouts := Client.Timeouts
                 (Receive => Duration'Value (GNAT.Command_Line.Parameter));

            when 'p' =>
               if GNAT.Command_Line.Full_Switch = "p" then
                  Pwd := To_Unbounded_String (GNAT.Command_Line.Parameter);

               elsif GNAT.Command_Line.Full_Switch = "pu" then
                  Proxy_User
                    := To_Unbounded_String (GNAT.Command_Line.Parameter);

               elsif GNAT.Command_Line.Full_Switch = "pp" then
                  Proxy_Pwd
                    := To_Unbounded_String (GNAT.Command_Line.Parameter);

               elsif GNAT.Command_Line.Full_Switch = "proxy" then
                  Proxy
                    := To_Unbounded_String (GNAT.Command_Line.Parameter);

               elsif GNAT.Command_Line.Full_Switch = "pa" then
                  Proxy_Auth
                    := Get_Auth_Mode (GNAT.Command_Line.Parameter);
               end if;

            when others =>
               raise Program_Error;         -- cannot occurs!
         end case;
      end loop;

      if Follow_Redirection and then Keep_Alive then
         Exceptions.Raise_Exception
           (Syntax_Error'Identity,
            "Follow redirection and keep-alive mode can't be used together.");
      end if;

      Get_Method : begin
         Method := Status.Request_Method'Value
           (GNAT.Command_Line.Get_Argument);
      exception
         when Constraint_Error =>
            Text_IO.Put_Line ("Method should be GET or PUT. See '" &
                             Ada.Command_Line.Command_Name & " -h'.");
            Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
            return False;
      end Get_Method;

      URL    := To_Unbounded_String (GNAT.Command_Line.Get_Argument);
      return True;
   end Parse_Command_Line;

   ----------------------
   -- Show_Certificate --
   ----------------------

   procedure Show_Certificate (Cert : Net.SSL.Certificate.Object) is
      use Ada.Text_IO;
      use type Net.SSL.Certificate.Object;
   begin
      if Cert = Net.SSL.Certificate.Undefined then
         Put_Line ("No certificate.");
      else
         Put_Line ("Subject : " & Net.SSL.Certificate.Subject (Cert));
         Put_Line ("Issuer  : " & Net.SSL.Certificate.Issuer (Cert));
      end if;
   end Show_Certificate;

   -----------
   -- Usage --
   -----------

   procedure Usage is
      use Ada.Text_IO;
   begin
      Text_IO.Put_Line ("Usage: agent [options] [GET/PUT] <URL>");
      Text_IO.Put_Line ("       -f           force display of message body.");
      Text_IO.Put_Line ("       -o           output result in file agent.out");
      Text_IO.Put_Line ("       -k           keep-alive connection.");
      Text_IO.Put_Line ("       -s           server-push mode.");
      Text_IO.Put_Line ("       -i           repeat over delay interval for"
                          & " stress test.");
      Text_IO.Put_Line ("       -r           follow redirection.");
      Text_IO.Put_Line ("       -d           debug mode, view HTTP headers.");
      Text_IO.Put_Line ("       -c           display server certificate.");
      Text_IO.Put_Line ("       -cc          Client certificate.");
      Text_IO.Put_Line ("       -sc          Set cookie.");
      Text_IO.Put_Line ("       -t           wait response timeout.");
      Text_IO.Put_Line ("       -proxy <proxy_url>");
      Text_IO.Put_Line ("       -u <user_name>");
      Text_IO.Put_Line ("       -p <password>");
      Text_IO.Put_Line ("       -a <www_authentication_mode"
                          & " (Any, Basic or Digest)>");
      Text_IO.Put_Line ("       -pu <proxy_user_name>");
      Text_IO.Put_Line ("       -pp <proxy_password>");
      Text_IO.Put_Line ("       -pa <proxy_authentication_mode"
                          & " (Any, Basic or Digest)>");
   end Usage;

   Data       : Response.Data;
   URL_Object : AWS.URL.Object;

begin
   if Ada.Command_Line.Argument_Count = 0 then
      Usage;
      return;
   elsif not Parse_Command_Line then
      return;
   end if;

   URL_Object := AWS.URL.Parse (To_String (URL));

   if not (Method = Status.GET and then Follow_Redirection) then
      Client.Create
        (Connection  => Connect,
         Host        => To_String (URL),
         Proxy       => To_String (Proxy),
         Persistent  => Keep_Alive,
         Server_Push => Server_Push,
         Certificate => To_String (Client_Cert),
         Timeouts    => Timeouts);

      Client.Set_Cookie (Connect, To_String (Cookie));

      Client.Set_WWW_Authentication
        (Connection => Connect,
         User       => To_String (User),
         Pwd        => To_String (Pwd),
         Mode       => WWW_Auth);

      Client.Set_Proxy_Authentication
        (Connection => Connect,
         User       => To_String (Proxy_User),
         Pwd        => To_String (Proxy_Pwd),
         Mode       => Proxy_Auth);

      if Show_Cert then
         Show_Certificate (Client.Get_Certificate (Connect));
      end if;
   end if;

   loop
      if Method = Status.GET then

         if Keep_Alive then
            Client.Get (Connect, Data);
         else
            Data := Client.Get
              (To_String (URL), To_String (User), To_String (Pwd),
               To_String (Proxy),
               To_String (Proxy_User), To_String (Proxy_Pwd),
               Follow_Redirection => Follow_Redirection,
               Certificate        => To_String (Client_Cert));
         end if;

      else
         --  ??? PUT just send a simple piece of Data.
         --  ??? would also be nice to handle POST request

         Client.Put (Connection => Connect,
                     Result     => Data,
                     Data       => "Un essai");
      end if;

      Text_IO.Put_Line
        ("Status Code = "
           & Messages.Image (Response.Status_Code (Data))
           & " - "
           & Messages.Reason_Phrase (Response.Status_Code (Data)));

      if Response.Status_Code (Data) = Messages.S301 then
         Text_IO.Put_Line ("New location : " & Response.Location (Data));
      end if;

      declare
         Cookie : constant String := Client.Get_Cookie (Connect);
      begin
         if Cookie /= "" then
            Text_IO.Put_Line ("Cookie: " & Cookie);
         end if;
      end;

      if MIME.Is_Text (Response.Content_Type (Data)) then

         if File then
            declare
               F : Text_IO.File_Type;
            begin
               Text_IO.Create (F, Text_IO.Out_File, "agent.out");
               Text_IO.Put_Line (F, Response.Message_Body (Data));
               Text_IO.Close (F);
            end;
         else
            Text_IO.New_Line;
            Text_IO.Put_Line (Response.Message_Body (Data));
         end if;

      else
         Text_IO.Put_Line
           (Messages.Content_Type (Response.Content_Type (Data)));

         if Response.Content_Length (Data) /= Response.Undefined_Length then
            Text_IO.Put_Line
              (Messages.Content_Length (Response.Content_Length (Data)));
         end if;

         if Force or else File then
            --  This is not a text/html body, but output it anyway

            declare
               use Streams;

               Message_Stream : Resources.File_Type;
               Buffer         : Stream_Element_Array (1 .. 4_096);
               Last           : Stream_Element_Offset;
            begin
               Response.Message_Body (Data, Message_Stream);

               if File then
                  declare
                     F : Streams.Stream_IO.File_Type;
                  begin
                     Stream_IO.Create
                       (F, Stream_IO.Out_File, "agent.out");

                     loop
                        Resources.Read (Message_Stream, Buffer, Last);

                        Stream_IO.Write (F, Buffer (1 .. Last));

                        exit when Last < Buffer'Last;
                     end loop;

                     Stream_IO.Close (F);
                  end;

               else
                  loop
                     Resources.Read (Message_Stream, Buffer, Last);

                     for K in Buffer'First .. Last loop
                        declare
                           C : constant Character
                             := Character'Val (Buffer (K));
                        begin
                           if C = ASCII.CR
                             or else C = ASCII.LF
                             or else not Characters.Handling.Is_Control (C)
                           then
                              Text_IO.Put (C);
                           else
                              Text_IO.Put ('.');
                           end if;
                        end;
                     end loop;

                     exit when Last < Buffer'Last;
                  end loop;
               end if;

               Resources.Close (Message_Stream);
            end;

         end if;
      end if;

      if Server_Push then
         loop
            declare
               Line : constant String
                 := Client.Read_Until (Connect, "" & ASCII.LF);
            begin
               exit when Line = "";
               Text_IO.Put (Line);
            end;
         end loop;
      end if;

      if Interval < Duration'Last then
         delay Interval;

      elsif Keep_Alive then
         --  check that the keep alive connection is kept alive

         Text_IO.Put_Line
           ("Type 'q' to exit, the connection will be closed.");

         Text_IO.Put_Line ("Any other key to retreive again the same URL");

         declare
            Char : Character;
         begin
            Text_IO.Get_Immediate (Char);
            exit when Char = 'q';
         end;

      else
         Client.Close (Connect);
         exit;
      end if;

   end loop;

exception
   when GNAT.Command_Line.Invalid_Switch =>
      Usage;
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

   when SE : Syntax_Error =>
      Text_IO.Put_Line ("Syntax error: " & Exceptions.Exception_Message (SE));

   when E : others =>
      Text_IO.Put_Line (Exceptions.Exception_Information (E));
end Agent;
