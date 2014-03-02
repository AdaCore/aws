------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2014, AdaCore                        --
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

--  Priority string test

pragma Ada_2012;

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Streams;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.Net.Buffered;
with AWS.Net.Log;
with AWS.Net.SSL;
with AWS.Utils;

with GNAT.Traceback.Symbolic;

procedure Priorities is

   use Ada.Text_IO;
   use AWS;

   use all type Ada.Strings.Unbounded.Unbounded_String;

   package ASU renames Ada.Strings.Unbounded;

   Server : Net.Socket_Type'Class := Net.Socket (False);
   Client : Net.SSL.Socket_Type;
   Sample : constant Ada.Streams.Stream_Element_Array (1 .. 100) :=
              (others => 1);
   Latest : constant Ada.Streams.Stream_Element_Array (Sample'Range) :=
              (others => 2);

   Config  : Net.SSL.Config;
   GNUTLS  : constant Boolean := Utils.Match (Net.SSL.Version, "GNUTLS");
   Verbose : Boolean := False with Atomic;
   Counter : Natural := 0;

   Previous_Chipher : ASU.Unbounded_String;

   Ciphers : ASU.Unbounded_String :=
               To_Unbounded_String (if GNUTLS then "NORMAL" else "DEFAULT");

   task Server_Task is
      entry Start;
   end Server_Task;

   function Is_Handshake_Error (Text : String) return Boolean;

   procedure Print (Text : String);
   --  Prints only in Verbose mode

   -----------
   -- Error --
   -----------

   procedure Error (Socket : Net.Socket_Type'Class; Message : String) is
      use GNAT.Traceback;
      Trace : Tracebacks_Array (1 .. 64);
      Last  : Natural;
   begin
      if Is_Handshake_Error (Message) then
         Put_Line ("Handshake error");
         Net.Log.Stop;
         return;
      end if;

      Call_Chain (Trace, Last);

      Put_Line
        ("# Network error: "
         & Message & Symbolic.Symbolic_Traceback (Trace (1 .. Last)));
   end Error;

   ------------------------
   -- Is_Handshake_Error --
   ------------------------

   function Is_Handshake_Error (Text : String) return Boolean is
   begin
      return Text = "No or insufficient priorities were set."
        or else Text = "No supported cipher suites have been found."
        or else Text = "14077410:SSL routines:SSL23_GET_SERVER_HELLO:"
                       & "sslv3 alert handshake failure";
   end Is_Handshake_Error;

   -----------
   -- Print --
   -----------

   procedure Print (Text : String) is
   begin
      if Verbose then
         Put_Line (Text);
      end if;
   end Print;

   -----------------
   -- Server_Task --
   -----------------

   task body Server_Task is
      Peer : Net.SSL.Socket_Type;
      Cfg  : Net.SSL.Config;
   begin
      Put_Line ("Server task start.");

      Server.Bind
        (Family => (if Net.IPv6_Available then Net.Family_Inet6
                                          else Net.Family_Inet),
         Port   => 0);

      Server.Listen;
      Server.Set_Timeout (2.0);
      Peer.Set_Timeout (1.0);

      Net.SSL.Initialize
        (Cfg,
         Certificate_Filename => "aws-server.crt",
         Key_Filename         => "aws-server.key",
         Exchange_Certificate => True,
         Trusted_CA_Filename  => "private-ca.crt");

      accept Start;

      loop
         Peer.Set_Config (Cfg);

         Peer.Set_Timeout (1.0);
         Server.Accept_Socket (Net.Socket_Type'Class (Peer));

         Print (Peer.Cipher_Description);

         declare
            use Ada.Streams;
            Buffer : Stream_Element_Array (Sample'Range);
         begin
            Net.Buffered.Read (Peer, Buffer);

            Peer.Shutdown;

            exit when Buffer = Latest;

            if Buffer /= Sample then
               Put_Line ("Data distortion.");
            end if;
         end;
      end loop;

      Server.Shutdown;
      Net.SSL.Release (Cfg);

      Put_Line ("Server task done.");

   exception
      when E : others =>
         Put_Line ("Server task " & Ada.Exceptions.Exception_Information (E));
   end Server_Task;

begin
   Net.Log.Start (Error => Error'Unrestricted_Access, Write => null);

   if Ada.Command_Line.Argument_Count > 0 then
      Verbose := True;
      Net.SSL.Set_Debug (Integer'Value (Ada.Command_Line.Argument (1)));
   end if;

   Print (Net.SSL.Version);

   Server_Task.Start;

   loop
      Net.SSL.Initialize
        (Config               => Config,
         Priorities           => To_String (Ciphers),
         Certificate_Filename => "aws-client.pem",
         Trusted_CA_Filename  => "private-ca.crt");

      Client.Set_Config (Config);

      Client.Set_Timeout (1.0);

      declare
         Host : constant String := Net.Localhost (Net.IPv6_Available);
         Port : constant Positive := Server.Get_Port;
      begin
         Client.Connect (Host, Port);
         Client.Send (Sample);
      exception
         when E : Net.Socket_Error =>
            Client.Shutdown;
            Net.SSL.Release (Config);

            if Is_Handshake_Error (Ada.Exceptions.Exception_Message (E)) then
               Net.SSL.Initialize
                 (Config               => Config,
                  Certificate_Filename => "aws-client.pem",
                  Trusted_CA_Filename  => "private-ca.crt");

               Client.Set_Config (Config);
               Client.Connect (Host, Port);
               Client.Send (Latest);
               Client.Shutdown;
               Net.SSL.Release (Config);
               exit;
            else
               raise;
            end if;
      end;

      Counter := Counter + 1;

      Append
        (Ciphers, ":-" & Utils.Head_Before (Client.Cipher_Description, " "));

      if To_String (Previous_Chipher) = Client.Cipher_Description then
         Put_Line ("The same cipher " & To_String (Previous_Chipher));
      else
         Previous_Chipher := To_Unbounded_String (Client.Cipher_Description);
      end if;

      Client.Shutdown;

      Net.SSL.Release (Config);
   end loop;

   Print ("Total disabled chipers" & Counter'Img);

   if Counter < 4 then
      Put_Line ("Too few iterations"  & Counter'Img);
   end if;

exception
   when E : others =>
      Put_Line ("Main task " & Ada.Exceptions.Exception_Information (E));
end Priorities;
