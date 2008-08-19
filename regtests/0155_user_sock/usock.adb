------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2006-2008, AdaCore                     --
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

with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

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
     (Container : in out U_Set; Timeout : in Duration; Count : out Natural);

   overriding function Status
     (Container : in U_Set; Index : in Positive) return Net.Event_Set;

   function "+" (Str : in String) return Unbounded_String
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

   function Socket (Security : in Boolean) return Net.Socket_Type'Class is
      S : U_Socket;
   begin
      return S;
   end Socket;

   ----------
   -- Bind --
   ----------

   overriding procedure Bind
     (Socket        : in out U_Socket;
      Port          : in     Natural;
      Host          : in     String := "";
      Reuse_Address : in     Boolean := False)
   is
      pragma Unreferenced (Socket, Port, Host);
   begin
      Text_IO.Put_Line ("Bind on U_Socket");
   end Bind;

   ------------
   -- Listen --
   ------------

   overriding procedure Listen
     (Socket     : in U_Socket;
      Queue_Size : in Positive := 5)
   is
      pragma Unreferenced (Socket, Queue_Size);
   begin
      Text_IO.Put_Line ("Listen on U_Socket");
   end Listen;

   -------------------
   -- Accept_Socket --
   -------------------

   overriding procedure Accept_Socket
     (Socket     : in     Net.Socket_Type'Class;
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
      Host   : in     String;
      Port   : in     Positive;
      Wait   : in     Boolean := True)
   is
      pragma Unreferenced (Socket, Host, Port, Wait);
   begin
      Text_IO.Put_Line ("Connect on U_Socket");
   end Connect;

   --------------
   -- Shutdown --
   --------------

   overriding procedure Shutdown
     (Socket : in U_Socket;
      How    : in Net.Shutmode_Type := Net.Shut_Read_Write)
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
     (Socket : in     U_Socket;
      Data   : in     Stream_Element_Array;
      Last   :    out Stream_Element_Offset)
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

         D (First + 9 .. First + 13) := "x.y.z";

         Text_IO.Put (D (D'First .. First + 13));
         Text_IO.Put_Line (D (Last .. D'Last));
      end;
      Text_IO.Put_Line ("--");
      Last := Data'Last;
   end Send;

   ---------------
   -- To_FD_Set --
   ---------------

   function To_FD_Set
     (Socket : in U_Socket;
      Events : in Net.Wait_Event_Set;
      Size   : in Positive := 1) return Net.FD_Set'Class
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
     (Container : in out U_Set; Timeout : in Duration; Count : out Natural)
   is
      pragma Unreferenced (Container, Timeout);
   begin
      Count := 1;
   end Wait;

   ------------
   -- Status --
   ------------

   function Status
     (Container : in U_Set; Index : in Positive) return Net.Event_Set
   is
      pragma Unreferenced (Container);
   begin
      return (Net.Input  => Done or Index /= 2,
              --  2 is the signal socket index inside of
              --  Net.Acceptors.Acceptor_Type.
              Net.Output => True,
              Net.Error  => Done);
   end Status;

   -------------
   -- Receive --
   -------------

   overriding procedure Receive
     (Socket : in     U_Socket;
      Data   :    out Stream_Element_Array;
      Last   :    out Stream_Element_Offset)
   is
      pragma Unreferenced (Socket);
   begin
      Text_IO.Put_Line ("Receive on U_Socket");

      if Done then
         Last := Data'First - 1;

      else
         M_Index := M_Index + 1;

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
     (Socket : in U_Socket) return Stream_Element_Count
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

   overriding function Get_FD (Socket : in U_Socket) return Integer is
   begin
      return 1;
   end Get_FD;

   ---------------
   -- Peer_Addr --
   ---------------

   overriding function Peer_Addr (Socket : in U_Socket) return String is
      pragma Unreferenced (Socket);
   begin
      --  Have to be 127.0.0.1 to cheat Socket_Pair
      return "127.0.0.1";
   end Peer_Addr;

   ---------------
   -- Peer_Port --
   ---------------

   overriding function Peer_Port (Socket : in U_Socket) return Positive is
      pragma Unreferenced (Socket);
   begin
      return 1;
   end Peer_Port;

   --------------
   -- Get_Addr --
   --------------

   overriding function Get_Addr (Socket : in U_Socket) return String is
   begin
      return "here";
   end Get_Addr;

   --------------
   -- Get_Port --
   --------------

   overriding function Get_Port (Socket : in U_Socket) return Positive is
      pragma Unreferenced (Socket);
   begin
      return 1;
   end Get_Port;

   --------------------------
   -- Set_Send_Buffer_Size --
   --------------------------

   overriding procedure Set_Send_Buffer_Size
     (Socket : in U_Socket;
      Size   : in Natural)
   is
      pragma Unreferenced (Socket, Size);
   begin
      raise Constraint_Error;
   end Set_Send_Buffer_Size;

   -----------------------------
   -- Set_Receive_Buffer_Size --
   -----------------------------

   overriding procedure Set_Receive_Buffer_Size
     (Socket : in U_Socket;
      Size   : in Natural)
   is
      pragma Unreferenced (Socket, Size);
   begin
      raise Constraint_Error;
   end Set_Receive_Buffer_Size;

   --------------------------
   -- Get_Send_Buffer_Size --
   --------------------------

   overriding function Get_Send_Buffer_Size
     (Socket : in U_Socket) return Natural
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
     (Socket : in U_Socket) return Natural
   is
      pragma Unreferenced (Socket);
   begin
      raise Constraint_Error;
      return 0;
   end Get_Receive_Buffer_Size;

   -----------
   -- Errno --
   -----------

   overriding function Errno (Socket : in U_Socket) return Integer is
      pragma Unreferenced (Socket);
   begin
      return 0;
   end Errno;

   --------
   -- CB --
   --------

   function CB (Request : in AWS.Status.Data) return Response.Data is
   begin
      Text_IO.Put_Line ("Callback... " & AWS.Status.URI (Request));
      return Response.Build (MIME.Text_HTML, "response from U_Socket");
   end CB;

end USock;
