------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                        Copyright (C) 2014, AdaCore                       --
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

--  Test for reuse session over session cache and over tickets TLS extension

with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Streams;

with AWS.Net.Log;
with AWS.Net.SSL;

with GNAT.Traceback.Symbolic;

procedure SSL_Reuse is

   use AWS;
   use Ada;
   use Ada.Streams;

   Server : Net.Socket_Type'Class := Net.Socket (False);
   Client : Net.SSL.Socket_Type;
   Config : Net.SSL.Config;
   SrvCfg : Net.SSL.Config;

   Sessions : array (1 .. 12) of Net.SSL.Session_Type;

   task Server_Side is
      entry Started;
   end Server_Side;

   -----------------
   -- Client_Side --
   -----------------

   task body Server_Side is
      Data    : Stream_Element_Array (1 .. 64);
      Last    : Stream_Element_Offset;
      Peer    : Net.SSL.Socket_Type;
      Rest    : Net.SSL.Socket_Type;
      Working : Boolean := True;
   begin
      Server.Bind (0, "localhost");
      Server.Listen;

      Net.SSL.Initialize (SrvCfg, "cert.pem", Ticket_Support => False);

      accept Started;

      while Working loop
         Peer.Set_Config (SrvCfg);
         Net.SSL.Accept_Socket (Server, Peer);

         Peer.Receive (Data, Last);

         if Last = 1 then
            if Data (1) = 1 then
               Net.SSL.Release (SrvCfg);
               Net.SSL.Initialize (SrvCfg, "cert.pem", Ticket_Support => True);
            else
               Working := False;
            end if;
         else
            Peer.Send (Data (1 .. Last));
         end if;

         Peer.Shutdown;
      end loop;

      Text_IO.Put_Line ("Server task done.");

   exception
      when E : others =>
         Text_IO.Put_Line (Exceptions.Exception_Information (E));
   end Server_Side;

   --------------------------
   -- Connect_Send_Receive --
   --------------------------

   procedure Connect_Send_Receive is
      Sample : constant Stream_Element_Array := (0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
   begin
      Client.Connect (Server.Get_Addr, Server.Get_Port);

      Client.Send (Sample);

      if Client.Receive /= Sample then
         Text_IO.Put_Line ("Data differ");
      end if;

      Text_IO.Put (' ' & Client.Session_Reused'Img);
   end Connect_Send_Receive;

   ---------------------------
   -- Create_Reuse_Sessions --
   ---------------------------

   procedure Create_Reuse_Sessions (Clear : Boolean := False) is
   begin
      for J in Sessions'Range loop
         Connect_Send_Receive;

         Sessions (J) := Client.Session_Data;

         Client.Shutdown;
      end loop;

      Text_IO.New_Line;

      Text_IO.Put_Line ("Reuse sessions");

      if Clear then
         Net.SSL.Clear_Session_Cache (SrvCfg);
      end if;

      for J in Sessions'Range loop
         Client.Set_Session_Data (Sessions (J));

         Connect_Send_Receive;

         if Net.SSL.Session_Id_Image (Sessions (J)) /= Client.Session_Id_Image
           and then Client.Session_Reused
         then
            Text_IO.Put_Line ("!!! Session not reused");
         end if;

         if Net.SSL.Session_Id_Image (Sessions (J)) = Client.Session_Id_Image
           and then not Client.Session_Reused
         then
            Text_IO.Put_Line ("!!! Session reused");
         end if;

         Client.Shutdown;
      end loop;
   end Create_Reuse_Sessions;

   -----------
   -- Error --
   -----------

   procedure Error (Socket : Net.Socket_Type'Class; Message : String) is
      use GNAT.Traceback;
      Trace : Tracebacks_Array (1 .. 64);
      Last  : Natural;
   begin
      Call_Chain (Trace, Last);

      Text_IO.Put_Line
        ("# Network error: "
         & Message & Symbolic.Symbolic_Traceback (Trace (1 .. Last)));
   end Error;

begin
   Net.Log.Start (Error => Error'Unrestricted_Access, Write => null);

   Server_Side.Started;

   Net.SSL.Initialize
     (Config, "", Ticket_Support => False, Security_Mode => Net.SSL.TLSv1_2);
   --  TLS 1.3 in GNUTLS has some difference with session resumption mechanism

   Client.Set_Config (Config);

   Text_IO.Put_Line ("Sessions creation no tickets no reuse");

   Create_Reuse_Sessions (Clear => True);
   Text_IO.Put_Line (Net.SSL.Session_Cache_Number (SrvCfg)'Img);

   Text_IO.Put_Line ("Sessions creation no tickets");
   Create_Reuse_Sessions;
   Text_IO.Put_Line (Net.SSL.Session_Cache_Number (SrvCfg)'Img);

   Net.SSL.Release (Config);
   Net.SSL.Initialize
     (Config, "", Ticket_Support => True, Security_Mode => Net.SSL.TLSv1_2);
   --  TLS 1.3 in GNUTLS has some difference with session resumption mechanism
   Client.Set_Config (Config);

   Text_IO.Put_Line ("Sessions creation client tickets");

   Create_Reuse_Sessions;
   Text_IO.Put_Line (Net.SSL.Session_Cache_Number (SrvCfg)'Img);

   Net.SSL.Release (Config);
   Net.SSL.Initialize
     (Config, "", Ticket_Support => False, Security_Mode => Net.SSL.TLSv1_2);
   --  TLS 1.3 in GNUTLS has some difference with session resumption mechanism
   Client.Set_Config (Config);

   Client.Connect (Server.Get_Addr, Server.Get_Port);
   Client.Send ((1 => 1));
   Client.Shutdown;

   Text_IO.Put_Line ("Sessions creation server tickets");

   Create_Reuse_Sessions;
   --  Text_IO.Put_Line (Net.SSL.Session_Cache_Number (SrvCfg)'Img);
   Text_IO.New_Line;
   --  Do not output Session_Cache_Number here because GNUTLS 3.2.4 turn on
   --  ticket support even if client side is not declared to support it.

   Net.SSL.Release (Config);
   Net.SSL.Initialize (Config, "", Ticket_Support => True);
   Client.Set_Config (Config);

   Text_IO.Put_Line ("Sessions creation with client and server tickets");

   Create_Reuse_Sessions (True);
   --  Text_IO.Put_Line (Net.SSL.Session_Cache_Number (SrvCfg)'Img);
   Text_IO.New_Line;
   --  Do not output Session_Cache_Number here because GNUTLS 3.0.28 put
   --  session into cache even if the ticket is using.

   Client.Connect (Server.Get_Addr, Server.Get_Port);
   Client.Send ((1 => 2));
   Client.Shutdown;

exception
   when E : others =>
      Text_IO.Put_Line (Exceptions.Exception_Information (E));
end SSL_Reuse;
