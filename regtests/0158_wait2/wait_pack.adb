------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2012, AdaCore                     --
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

with Ada.Exceptions;
with Ada.Streams;
with Ada.Text_IO;
with AWS.Net.Generic_Sets;
with AWS.Net.SSL;

with Stack_Size;

package body Wait_Pack is

   use Ada.Streams;
   use AWS;

   type Null_Record is null record;

   package Sets is new Net.Generic_Sets (Null_Record);

   ---------
   -- Run --
   ---------

   procedure Run (Security : Boolean) is
      use Ada.Text_IO;
      use type Sets.Socket_Count;

      Set_Size    : constant := 20;
      Sample_Size : constant := 10;

      Server      : Net.Socket_Type'Class := Net.Socket (False);

      Index       : Sets.Socket_Index := 1;

      task Client_Side is
         pragma Storage_Size (Stack_Size.Value);
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
               Socket.Connect
                 (Net.Localhost (Server.Is_IPv6), Server.Get_Port);

               accept Next;
               delay A_Bit;
               Socket.Send
                 ((1 .. Sample_Size => Stream_Element (J rem 256)));

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
               Socket.Send
                 ((1 .. Sample_Size =>
                     Stream_Element (Sets.Count (Set) rem 256)));

               accept Next;
               delay A_Bit;
               Socket.Shutdown;
            end;
         end loop;

      exception
         when E : others =>
            Put_Line
              ("Client side " & Ada.Exceptions.Exception_Information (E));
      end Client_Side;

      Set : Sets.Socket_Set_Type;

   begin
      Server.Bind (0);
      Server.Listen;
      Server.Set_Blocking_Mode (False);

      Sets.Add (Set, Server, Sets.Input);

      for J in 1 .. Set_Size * 4 loop
         Client_Side.Next;
         Sets.Wait (Set, 1.0);

         Index := 1;

         Sets.Next (Set, Index);

         if not Sets.In_Range (Set, Index)
           or else not Sets.Is_Read_Ready (Set, Index)
         then
            Put_Line ("Could not read from socket.");
            exit;
         end if;

         declare
            Socket         : Net.Socket_Type'Class :=
                               Sets.Get_Socket (Set, Index);
            New_Sock       : Net.Socket_Type'Class := Net.Socket (Security);
            Socket_Removed : Boolean := False;
         begin
            if Socket.Get_FD = Server.Get_FD then
               Put_Line ("Accept" & Integer'Image ((J + 1) / 2));
               Server.Accept_Socket (New_Socket => New_Sock);

               New_Sock.Set_Blocking_Mode (False);

               Sets.Add (Set, New_Sock, Sets.Input);

            else
               declare
                  Data : Stream_Element_Array (1 .. Sample_Size);
               begin
                  Data := Socket.Receive;

                  Put ("Data");

                  for J in Data'Range loop
                     Put (Stream_Element_Offset'Image (J)
                          & " =>" & Stream_Element'Image (Data (J)));
                  end loop;

                  New_Line;
               exception
                  when E : Net.Socket_Error =>
                     Put_Line ("Close socket.");

                     Socket.Shutdown;
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

      Server.Shutdown;

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
