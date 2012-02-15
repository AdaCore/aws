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
with AWS.Net.Sets;
with AWS.Net.SSL;

with Stack_Size;

procedure Wait_Proc (Security : Boolean) is

   use Ada.Exceptions;
   use Ada.Streams;
   use Ada.Text_IO;
   use AWS;
   use AWS.Net;

   use type Sets.Socket_Count;

   Set_Size    : constant := 20;
   Sample_Size : constant := 10;

   Server : Net.Socket_Type'Class := Net.Socket (False);

   task Client_Side is
      pragma Storage_Size (Stack_Size.Value);
      entry Start;
   end Client_Side;

   -----------------
   -- Client_Side --
   -----------------

   task body Client_Side is
      use type Ada.Streams.Stream_Element;
      Set   : Sets.Socket_Set_Type;
      Index : Sets.Socket_Index;
      Data  : Stream_Element_Array (1 .. Sample_Size);
      Sum   : Stream_Element := 0;
   begin
      accept Start;

      for J in 1 .. Set_Size loop
         declare
            Socket : Net.Socket_Type'Class := Net.Socket (Security);
         begin
            Socket.Set_Timeout (1.0);

            Socket.Connect (Server.Get_Addr, Server.Get_Port, Wait => False);

            Sets.Add (Set, Socket, Sets.Output);
         end;
      end loop;

      Main : while Sets.Count (Set) > 0 loop
         Sets.Wait (Set, 2.0);

         Index := 1;

         loop
            Sets.Next (Set, Index);

            exit when not Sets.In_Range (Set, Index);

            declare
               Socket : Socket_Type'Class := Sets.Get_Socket (Set, Index);
            begin
               if Sets.Is_Write_Ready (Set, Index) then
                  Sets.Set_Mode (Set, Index, Sets.Input);

                  if Security then
                     SSL.Do_Handshake (SSL.Socket_Type (Socket));
                  end if;

                  Index := Index + 1;

               elsif Sets.Is_Read_Ready (Set, Index) then
                  begin
                     Data  := Socket.Receive;

                     Sum := Sum + Data (Data'First);

                     Put_Line ("Read");

                     Index := Index + 1;

                  exception
                     when Net.Socket_Error =>
                        Net.Shutdown (Socket);
                        Sets.Remove_Socket (Set, Index);
                  end;

               elsif Sets.Is_Error (Set, Index) then
                  Put_Line ("Socket error" & Integer'Image (Errno (Socket)));

                  Socket.Shutdown;
                  Sets.Remove_Socket (Set, Index);

               else
                  Put_Line ("Wait error.");
                  exit Main;
               end if;
            end;
         end loop;
      end loop Main;

      Put_Line (Stream_Element'Image (Sum));

   exception
      when E : others =>
         Put_Line ("Client side " & Exception_Information (E));
   end Client_Side;

begin
   Server.Bind (Host => "localhost", Port => 0);
   Server.Listen (Set_Size);

   Server.Set_Timeout (1.0);

   Client_Side.Start;

   for J in 1 .. Set_Size loop
      declare
         New_Sock : Net.Socket_Type'Class := Net.Socket (Security);
      begin
         Server.Accept_Socket (New_Socket => New_Sock);

         New_Sock.Send ((1 .. Sample_Size => Stream_Element (J)));

         delay 0.5;
         New_Sock.Shutdown;
      end;
   end loop;

   Server.Shutdown;

exception
   when E : others =>
      Put_Line ("Server side " & Exception_Information (E));
end Wait_Proc;
