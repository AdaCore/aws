------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2007-2012, AdaCore                     --
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
with Ada.Streams;
with Ada.Text_IO;
with AWS.Net.Sets;

procedure Max_Poll_Size is
   use AWS.Net;
   use Ada.Streams;
   use Ada.Text_IO;

   use type Sets.Socket_Count;

   Set                  : Sets.Socket_Set_Type;
   Cnt                  : Sets.Socket_Count;
   Server, Client, Peer : Socket_Type'Class := Socket (False);
   Data                 : constant Stream_Element_Array :=
     (1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15, 16, 17, 19, 20, 21, 22);

begin
   Server.Bind (0);
   Server.Listen;

   loop
      begin
         Client.Connect (Localhost (Server.Is_IPv6), Server.Get_Port);
         Server.Accept_Socket (Peer);
      exception
         when E : Socket_Error =>
            Put_Line (Ada.Exceptions.Exception_Information (E));
            exit;
      end;

      Peer.Set_Timeout (0.0);
      Client.Set_Timeout (0.0);

      Sets.Add (Set, Peer, Sets.Input);
      Sets.Add (Set, Client, Sets.Input);
   end loop;

   Put_Line (Sets.Count (Set)'Img);

   for J in 1 .. Sets.Count (Set) loop
      Sets.Get_Socket (Set, J - 1 + J rem 2 * 2).Send (Data);

      Sets.Wait (Set, 0.25, Cnt);

      if Cnt /= 1 then
         Put_Line ("Wrong Cnt 1 /=" & Cnt'Img);
      end if;

      if Sets.Is_Read_Ready (Set, J) then
         if Sets.Get_Socket (Set, J).Receive /= Data then
            Put_Line ("Wrong data");
         end if;
      else
         Put_Line ("Wrong ready state");
      end if;
   end loop;

   Put_Line ("One socket active test done");

   for J in 1 .. Sets.Count (Set) / 2 loop
      Sets.Get_Socket (Set, J * 2).Send (Data);
   end loop;

   Sets.Wait (Set, 0.25, Cnt);

   if Cnt /= Sets.Count (Set) / 2 then
      Put_Line ("Wrong count " & Cnt'Img);
   end if;

   for J in 1 .. Sets.Count (Set) loop
      if Sets.Is_Read_Ready (Set, J) = (J rem 2 = 0) then
         Put_Line ("Wrong Ready state");
      end if;

      if J rem 2 = 1 and then Sets.Get_Socket (Set, J).Receive /= Data then
         Put_Line ("Wrong Data");
      end if;
   end loop;

   Put_Line ("Half sockets active test done");
end Max_Poll_Size;
