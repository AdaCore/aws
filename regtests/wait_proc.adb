------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2004                            --
--                               ACT-Europe                                 --
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

with Ada.Exceptions;
with Ada.Streams;
with Ada.Text_IO;
with AWS.Net.Sets;

procedure Wait_Proc (Security : Boolean; Port : Positive) is
   use Ada.Text_IO;
   use AWS;

   use type Net.Sets.Socket_State;

   Set_Size    : constant := 20;
   Sample_Size : constant := 10;

   task Client_Side is
      entry Next;
   end Client_Side;

   procedure Set_Small_Buffers (Socket : in out Net.Socket_Type'Class);

   -----------------
   -- Client_Side --
   -----------------

   task body Client_Side is
      Set    : Net.Sets.Socket_Set_Type;
      A_Bit  : constant Duration := 0.125;
   begin
      for J in 1 .. Set_Size loop
         declare
            Socket : Net.Socket_Type'Class := Net.Socket (Security);
         begin
            accept Next;
            delay A_Bit;
            Net.Connect (Socket, "localhost", Port);

            Set_Small_Buffers (Socket);

            accept Next;
            delay A_Bit;
            Net.Send
              (Socket,
               (1 .. Sample_Size => Ada.Streams.Stream_Element (J rem 256)));

            Net.Sets.Add (Set, Socket, Net.Sets.Output);
         end;
      end loop;

      Net.Sets.Wait (Set, 2.0);

      --  All sockets should be ready for output.

      while Net.Sets.Get_Socket_State (Set) = Net.Sets.Output loop
         declare
            Socket : Net.Socket_Type'Class := Net.Sets.Get_Socket (Set);
         begin
            Net.Sets.Remove_Socket (Set);

            accept Next;
            delay A_Bit;
            Net.Send
              (Socket,
               (1 .. Sample_Size => Ada.Streams.Stream_Element
                                      (Net.Sets.Count (Set) rem 256)));

            accept Next;
            delay A_Bit;
            Net.Shutdown (Socket);
            Net.Free (Socket);
         end;
      end loop;

   exception
      when E : others =>
         Put_Line ("Client side " & Ada.Exceptions.Exception_Information (E));
   end Client_Side;

   -----------------------
   -- Set_Small_Buffers --
   -----------------------

   procedure Set_Small_Buffers (Socket : in out Net.Socket_Type'Class) is
   begin
      if not Security then
         Net.Set_Send_Buffer_Size    (Socket, 64);
         Net.Set_Receive_Buffer_Size (Socket, 64);
      end if;
   end Set_Small_Buffers;

   Set    : Net.Sets.Socket_Set_Type;
   Server : Net.Socket_Type'Class := Net.Socket (False);

begin
   Net.Bind (Server, Port);
   Net.Listen (Server);
   Net.Set_Blocking_Mode (Server, False);

   Net.Sets.Add (Set, Server, Net.Sets.Input);

   for J in 1 .. Set_Size * 4 loop
      Client_Side.Next;
      Net.Sets.Wait (Set, 1.0);

      if Net.Sets.Get_Socket_State (Set) /= Net.Sets.Input then
         Put_Line ("State " & Net.Sets.Socket_State'Image
                                (Net.Sets.Get_Socket_State (Set))
                   & " /= Input");
         exit;
      end if;

      declare
         Socket : Net.Socket_Type'Class := Net.Sets.Get_Socket (Set);
         New_Sock : Net.Socket_Type'Class := Net.Socket (Security);
         Socket_Removed : Boolean := False;
      begin
         if Net.Get_FD (Socket) = Net.Get_FD (Server) then
            Put_Line ("Accept" & Integer'Image ((J + 1) / 2));
            Net.Accept_Socket (Server, New_Socket => New_Sock);

            Set_Small_Buffers (New_Sock);

            Net.Set_Blocking_Mode (New_Sock, False);

            Net.Sets.Add (Set, New_Sock, Net.Sets.Input);
         else
            declare
               Data : Ada.Streams.Stream_Element_Array (1 .. Sample_Size);
            begin
               Data := Net.Receive (Socket);

               Put ("Data");

               for J in Data'Range loop
                  Put (Ada.Streams.Stream_Element_Offset'Image (J)
                       & " =>" & Ada.Streams.Stream_Element'Image (Data (J)));
               end loop;

               New_Line;
            exception
               when E : Net.Socket_Error =>
                  Put_Line ("Close socket.");

                  Net.Shutdown (Socket);
                  Net.Free (Socket);
                  Net.Sets.Remove_Socket (Set);

                  Socket_Removed := True;
            end;
         end if;

         if not Socket_Removed then
            Net.Sets.Next (Set);
         end if;
      end;

      if Net.Sets.Get_Socket_State (Set) /= Net.Sets.None then
         Put_Line ("State " & Net.Sets.Socket_State'Image
                                (Net.Sets.Get_Socket_State (Set))
                   & " /= None");
         exit;
      end if;
   end loop;

   Net.Shutdown (Server);

   abort Client_Side;
end Wait_Proc;
