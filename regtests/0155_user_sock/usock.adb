------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2006-2016, AdaCore                     --
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

with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.OS_Lib;

with AWS.MIME;
with AWS.Net.Poll_Events;
with AWS.Translator;

package body USock is

   use Ada;
   use Ada.Strings.Unbounded;

   Chunk_Size : constant := 10;

   Done       : Boolean := False;

   type UString is array (Positive range <>) of Unbounded_String;

   type U_Set is new AWS.Net.Poll_Events.Set with null record;

   overriding procedure Wait
     (Container : in out U_Set; Timeout : Duration; Count : out Natural);

   overriding function Status
     (Container : U_Set; Index : Positive) return Net.Event_Set;

   function "+" (Str : String) return Unbounded_String
     renames To_Unbounded_String;

   CRLF       : constant String := ASCII.CR & ASCII.LF;

   Message    : UString :=
                  (+"GET /toto HTTP/1.0" & CRLF,
                   +"Content-length: 1" & CRLF,
                   +"" & CRLF,
                   +"a");

   M_Index : Natural := 0;

   protected Data is

      entry Ready;

      procedure Shutdown;

   private

      N_Data : Natural := 1;
      Stop   : Boolean := False;

   end Data;

   ----------
   -- Data --
   ----------

   protected body Data is

      -----------
      -- Ready --
      -----------

      entry Ready when N_Data > 0 or else Stop is
      begin
         if N_Data > 0 then
            N_Data := N_Data - 1;
         end if;
      end Ready;

      ----------
      -- Stop --
      ----------

      procedure Shutdown is
      begin
         Stop := True;
      end Shutdown;

   end Data;

   ------------
   -- Socket --
   ------------

   function Socket (Security : Boolean) return Net.Socket_Type'Class is
      S : U_Socket;
   begin
      return S;
   end Socket;

   ----------
   -- Bind --
   ----------

   overriding procedure Bind
     (Socket        : in out U_Socket;
      Port          : Natural;
      Host          : String := "";
      Reuse_Address : Boolean := False;
      IPv6_Only     : Boolean := False;
      Family        : Net.Family_Type := Net.Family_Unspec)
   is
      pragma Unreferenced
        (Socket, Port, Host, Reuse_Address, IPv6_Only, Family);
   begin
      Socket.Server := True;
      Text_IO.Put_Line ("Bind on U_Socket");
   end Bind;

   ------------
   -- Listen --
   ------------

   overriding procedure Listen
     (Socket     : U_Socket;
      Queue_Size : Positive := 5)
   is
      pragma Unreferenced (Socket, Queue_Size);
   begin
      Text_IO.Put_Line ("Listen on U_Socket");
   end Listen;

   -------------------
   -- Accept_Socket --
   -------------------

   overriding procedure Accept_Socket
     (Socket     : Net.Socket_Type'Class;
      New_Socket : in out U_Socket)
   is
      pragma Unreferenced (Socket, New_Socket);
   begin
      delay 0.1;
      Data.Ready;

      if Done then
         --  We are done, the server will be shutdown, wait a bit to be sure
         --  the shutdown process has started.

         delay 1.0;
         raise Net.Socket_Error;
      end if;

      Text_IO.Put_Line ("Accept_Socket on U_Socket");
   end Accept_Socket;

   -------------
   -- Connect --
   -------------

   overriding procedure Connect
     (Socket : in out U_Socket;
      Host   : String;
      Port   : Positive;
      Wait   : Boolean := True;
      Family : Net.Family_Type := Net.Family_Unspec)
   is
      pragma Unreferenced (Socket, Host, Port, Wait, Family);
   begin
      Text_IO.Put_Line ("Connect on U_Socket");
   end Connect;

   ------------------
   -- Is_Listening --
   ------------------

   overriding function Is_Listening (Socket : U_Socket) return Boolean is
   begin
      return Socket.Server;
   end Is_Listening;

   --------------
   -- Shutdown --
   --------------

   overriding procedure Shutdown
     (Socket : U_Socket;
      How    : Net.Shutmode_Type := Net.Shut_Read_Write)
   is
      pragma Unreferenced (Socket);
   begin
      Text_IO.Put_Line ("Shutdown on U_Socket");
      Data.Shutdown;
   end Shutdown;

   ----------
   -- Send --
   ----------

   overriding procedure Send
     (Socket : U_Socket;
      Data   : Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      pragma Unreferenced (Socket);
   begin
      Text_IO.Put_Line ("Got : --");
      declare
         Letter_Number : constant Strings.Maps.Character_Set :=
                           Strings.Maps.To_Set
                             ("0123456789abcdefghijklmnopqrstuvwxyz"
                              & "ABCDEFGHIJKLMNOPQRSTUVWXYZ");
         D           : String := Translator.To_String (Data);
         First, Last : Natural := 0;
      begin
         First := Strings.Fixed.Index (D, "Date");
         Last  := Strings.Fixed.Index (D, "GMT");

         for K in First  + 4 .. Last + 2 loop
            if Strings.Maps.Is_In (D (K), Letter_Number) then
               D (K) := 'x';
            end if;
         end loop;

         First := Strings.Fixed.Index (D, "Server) v");
         Last := Strings.Fixed.Index (D (First .. D'Last), (1 => ASCII.CR));

         D (First + 9 .. First + 11) := "x.y";

         Text_IO.Put (D (D'First .. First + 11));
         Text_IO.Put_Line (D (Last .. D'Last));
      end;
      Text_IO.Put_Line ("--");
      Last := Data'Last;
   end Send;

   ---------------
   -- To_FD_Set --
   ---------------

   function To_FD_Set
     (Socket : U_Socket;
      Events : Net.Wait_Event_Set;
      Size   : Positive := 1) return Net.FD_Set'Class
   is
      Result : U_Set (Size);
   begin
      Add (Result, Get_FD (Socket), Events);
      return Result;
   end To_FD_Set;

   ----------
   -- Wait --
   ----------

   procedure Wait
     (Container : in out U_Set; Timeout : Duration; Count : out Natural)
   is
      pragma Unreferenced (Container, Timeout);
   begin
      Count := 1;
   end Wait;

   ------------
   -- Status --
   ------------

   function Status
     (Container : U_Set; Index : Positive) return Net.Event_Set
   is
      pragma Unreferenced (Container);
   begin
      return (Net.Input  => Done or Index /= 1,
              --  1 is the signal socket index inside of
              --  Net.Acceptors.Acceptor_Type.
              Net.Output => True,
              Net.Error  => Done);
   end Status;

   -------------
   -- Receive --
   -------------

   overriding procedure Receive
     (Socket : U_Socket;
      Data   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      pragma Unreferenced (Socket);
   begin
      Text_IO.Put_Line ("Receive on U_Socket");

      if Done then
         Last := Data'First - 1;

         Text_IO.Put_Line ("Too many time on Receive, exit now!");
         GNAT.OS_Lib.OS_Exit (1);

      else
         M_Index := M_Index + 1;

         if M_Index > Message'Last then
            Text_IO.Put_Line ("Too many time on Receive, exit now!");
            GNAT.OS_Lib.OS_Exit (1);
         end if;

         declare
            M : constant String := To_String (Message (M_Index));
         begin
            Last := Data'First + M'Length - 1;
            Data (Data'First .. Last) :=
              Translator.To_Stream_Element_Array (M);

            if M_Index = Message'Last then
               Done := True;
            end if;
         end;
      end if;
   end Receive;

   -------------
   -- Pending --
   -------------

   overriding function Pending
     (Socket : U_Socket) return Stream_Element_Count
   is
      pragma Unreferenced (Socket);
   begin
      if Done then
         return 0;
      else
         return Chunk_Size;
      end if;
   end Pending;

   ------------
   -- Get_FD --
   ------------

   overriding function Get_FD (Socket : U_Socket) return Integer is
   begin
      return 1;
   end Get_FD;

   ---------------
   -- Peer_Addr --
   ---------------

   overriding function Peer_Addr (Socket : U_Socket) return String is
      pragma Unreferenced (Socket);
   begin
      --  Have to be 127.0.0.1 to cheat Socket_Pair
      return "127.0.0.1";
   end Peer_Addr;

   ---------------
   -- Peer_Port --
   ---------------

   overriding function Peer_Port (Socket : U_Socket) return Positive is
      pragma Unreferenced (Socket);
   begin
      return 1;
   end Peer_Port;

   --------------
   -- Get_Addr --
   --------------

   overriding function Get_Addr (Socket : U_Socket) return String is
   begin
      return "here";
   end Get_Addr;

   --------------
   -- Get_Port --
   --------------

   overriding function Get_Port (Socket : U_Socket) return Positive is
      pragma Unreferenced (Socket);
   begin
      return 1;
   end Get_Port;

   --------------------------
   -- Set_Send_Buffer_Size --
   --------------------------

   overriding procedure Set_Send_Buffer_Size
     (Socket : U_Socket;
      Size   : Natural)
   is
      pragma Unreferenced (Socket, Size);
   begin
      raise Constraint_Error;
   end Set_Send_Buffer_Size;

   -----------------------------
   -- Set_Receive_Buffer_Size --
   -----------------------------

   overriding procedure Set_Receive_Buffer_Size
     (Socket : U_Socket;
      Size   : Natural)
   is
      pragma Unreferenced (Socket, Size);
   begin
      raise Constraint_Error;
   end Set_Receive_Buffer_Size;

   --------------------------
   -- Get_Send_Buffer_Size --
   --------------------------

   overriding function Get_Send_Buffer_Size
     (Socket : U_Socket) return Natural
   is
      pragma Unreferenced (Socket);
   begin
      raise Constraint_Error;
      return 0;
   end Get_Send_Buffer_Size;

   -----------------------------
   -- Get_Receive_Buffer_Size --
   -----------------------------

   overriding function Get_Receive_Buffer_Size
     (Socket : U_Socket) return Natural
   is
      pragma Unreferenced (Socket);
   begin
      raise Constraint_Error;
      return 0;
   end Get_Receive_Buffer_Size;

   -----------
   -- Errno --
   -----------

   overriding function Errno (Socket : U_Socket) return Integer is
      pragma Unreferenced (Socket);
   begin
      return 0;
   end Errno;

   --------
   -- CB --
   --------

   function CB (Request : AWS.Status.Data) return Response.Data is
   begin
      Text_IO.Put_Line ("Callback... " & AWS.Status.URI (Request));
      return Response.Build (MIME.Text_HTML, "response from U_Socket");
   end CB;

end USock;
