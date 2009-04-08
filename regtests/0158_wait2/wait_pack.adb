------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2009, AdaCore                     --
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

with Ada.Exceptions;
with Ada.Streams;
with Ada.Text_IO;
with AWS.Net.Generic_Sets;
with AWS.Net.SSL;

with Get_Free_Port;

package body Wait_Pack is

   use Ada.Streams;
   use AWS;

   type Null_Record is null record;

   package Sets is new Net.Generic_Sets (Null_Record);

   ---------
   -- Run --
   ---------

   procedure Run (Security : Boolean; Port : Positive) is
      use Ada.Text_IO;
      use type Sets.Socket_Count;

      Set_Size    : constant := 20;
      Sample_Size : constant := 10;

      Free_Port   : Positive := Port;

      Index       : Sets.Socket_Index := 1;

      task Client_Side is
         entry Next;
      end Client_Side;

      -----------------
      -- Client_Side --
      -----------------

      task body Client_Side is
         A_Bit : constant Duration := 0.125;
         Set   : Sets.Socket_Set_Type;
      begin
         for J in 1 .. Set_Size loop
            declare
               Socket : Net.Socket_Type'Class := Net.Socket (Security);
            begin
               accept Next;
               delay A_Bit;
               Net.Connect (Socket, "localhost", Free_Port);

               accept Next;
               delay A_Bit;
               Net.Send
                 (Socket,
                  (1 .. Sample_Size => Stream_Element (J rem 256)));

               Sets.Add (Set, Socket, Sets.Output);
            end;
         end loop;

         Sets.Wait (Set, 2.0);

         --  All sockets should be ready for output

         while Sets.Count (Set) > 0 and then Sets.Is_Write_Ready (Set, 1) loop
            declare
               Socket : Net.Socket_Type'Class := Sets.Get_Socket (Set, 1);
            begin
               Sets.Remove_Socket (Set, 1);

               accept Next;
               delay A_Bit;
               Net.Send
                 (Socket,
                  (1 .. Sample_Size =>
                     Stream_Element (Sets.Count (Set) rem 256)));

               accept Next;
               delay A_Bit;
               Net.Shutdown (Socket);
            end;
         end loop;

      exception
         when E : others =>
            Put_Line
              ("Client side " & Ada.Exceptions.Exception_Information (E));
      end Client_Side;

      Set    : Sets.Socket_Set_Type;
      Server : Net.Socket_Type'Class := Net.Socket (False);

   begin
      Get_Free_Port (Free_Port);

      Net.Bind (Server, Free_Port);
      Net.Listen (Server);
      Net.Set_Blocking_Mode (Server, False);

      Sets.Add (Set, Server, Sets.Input);

      for J in 1 .. Set_Size * 4 loop
         Client_Side.Next;
         Sets.Wait (Set, 1.0);

         Index := 1;

         Sets.Next (Set, Index);

         if not Sets.Is_Read_Ready (Set, Index) then
            Put_Line ("Could not read from socket.");
            exit;
         end if;

         declare
            Socket         : Net.Socket_Type'Class
              := Sets.Get_Socket (Set, Index);
            New_Sock       : Net.Socket_Type'Class := Net.Socket (Security);
            Socket_Removed : Boolean := False;
         begin
            if Net.Get_FD (Socket) = Net.Get_FD (Server) then
               Put_Line ("Accept" & Integer'Image ((J + 1) / 2));
               Net.Accept_Socket (Server, New_Socket => New_Sock);

               Net.Set_Blocking_Mode (New_Sock, False);

               Sets.Add (Set, New_Sock, Sets.Input);
            else
               declare
                  Data : Stream_Element_Array (1 .. Sample_Size);
               begin
                  Data := Net.Receive (Socket);

                  Put ("Data");

                  for J in Data'Range loop
                     Put (Stream_Element_Offset'Image (J)
                          & " =>" & Stream_Element'Image (Data (J)));
                  end loop;

                  New_Line;
               exception
                  when E : Net.Socket_Error =>
                     Put_Line ("Close socket.");

                     Net.Shutdown (Socket);
                     Sets.Remove_Socket (Set, Index);

                     Socket_Removed := True;
               end;
            end if;

            if not Socket_Removed then
               Index := Index + 1;
               Sets.Next (Set, Index);
            end if;
         end;

      end loop;

      Net.Shutdown (Server);

      --  abort Client_Side;
      --  The task Client_Side terminates itself without abort statement.
      --  The abort statement above causes an error on Win32 platform after
      --  introducing the SSL.Locking package.
      --  Probably a regression!

   exception
      when E : others =>
         Put_Line
           ("Server side " & Ada.Exceptions.Exception_Information (E));
   end Run;

end Wait_Pack;
