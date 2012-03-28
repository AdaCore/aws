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

with Ada.Real_Time;
with Ada.Exceptions;
with Ada.Streams;
with Ada.Text_IO;
with AWS.Net.Sets;
with AWS.Net.Std;
with AWS.OS_Lib;
with AWS.Utils;

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

   Error_Count : Integer := 16;
   OK_Count    : Integer := 16;
   OK_Previous : Boolean := True;

   Set_Size : constant := 4096;
   type FD_Set_Type is array (0 .. Set_Size - 1) of Boolean;
   pragma Pack (FD_Set_Type);
   for FD_Set_Type'Size use Set_Size;

   FDS : FD_Set_Type;

   task Writer_Task is
      entry Delayed_Send (Index : Sets.Socket_Index);
      entry Cancel_Delay;
   end Writer_Task;

   procedure Read_Data (Socket : Socket_Type'Class; Mark : String) is
   begin
      if Socket.Receive /= Data then
         Put_Line ("Wrong data " & Mark);
      end if;
   end Read_Data;

   procedure Read_Data (Idx : Sets.Socket_Index; Mark : String) is
   begin
      Read_Data (Sets.Get_Socket (Set, Idx), Mark);
   end Read_Data;

   -----------------
   -- Writer_Task --
   -----------------

   task body Writer_Task is
      Index : Sets.Socket_Index;
   begin
      loop
         accept Delayed_Send (Index : Sets.Socket_Index) do
            Writer_Task.Index := Delayed_Send.Index;
         end Delayed_Send;

         select
            accept Cancel_Delay;
         or delay 0.25;
            Sets.Get_Socket (Set, Index).Send (Data);
         end select;
      end loop;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
   end Writer_Task;

begin
   Server.Bind (0);
   Server.Listen;

   for J in FDS'Range loop
      FDS (J) := False;
   end loop;

   loop
      begin
         Client.Connect (Localhost (Server.Is_IPv6), Server.Get_Port);
         Server.Accept_Socket (Peer);
      exception
         when E : Socket_Error =>
            Put_Line
              ("At connection " & Ada.Exceptions.Exception_Information (E));
            Put_Line ("Wait count" & Sets.Count (Set)'Img);
            exit;
      end;

      Peer.Set_Timeout (0.0);
      Client.Set_Timeout (0.0);

      Client.Send (Data);
      Peer.Send (Data);

      Read_Data (Peer, "peer");
      Read_Data (Client, "client");

      Sets.Add (Set, Peer, Sets.Input);
      Sets.Add (Set, Client, Sets.Input);
   end loop;

   loop
      declare
         Socket : Socket_Access;
      begin
         Sets.Wait (Set, 0.0, Cnt);
         exit;
      exception
         when E : Socket_Error =>
            declare
               Prefix : constant String := "Poll (Size => ";
               Suffix :  constant String := ") error code 10024";
               EM : constant String := Ada.Exceptions.Exception_Message (E);
            begin
               if EM'Length > Prefix'Length + Suffix'Length
                 and then EM (Prefix'Range) = Prefix
                 and then EM (EM'Last - Suffix'Length + 1 .. EM'Last) = Suffix
               then
                  if OK_Previous then
                     Put_Line
                       ("Too many sockets for poll "
                        & EM (Prefix'Last + 1 .. EM'Last - Suffix'Length));
                     OK_Previous := False;
                  end if;
               else
                  Put_Line ("At wait " & EM);
               end if;
            end;

            for J in 1 .. 2 loop
               Sets.Remove_Socket (Set, Sets.Count (Set), Socket);
               Socket.Shutdown;
               Free (Socket);
            end loop;
      end;
   end loop;

   OK_Previous := True;

   Put_Line ("Wait count" & Sets.Count (Set)'Img);

   --  Test read timeout is not depend on others socket activities.
   --  Actual for poll over posix select implementation.

   for J in reverse 1 .. Sets.Count (Set) loop
      declare
         use Ada.Real_Time;
         Peer_Idx : constant Sets.Socket_Index := J - 1 + J rem 2 * 2;
         Socket   : aliased Socket_Type'Class := Sets.Get_Socket (Set, J);
         Enought  : Boolean := False;
      begin
         for K in 1 .. Sets.Count (Set) loop
            --  Write into all sockets except the peer one
            if K /= Peer_Idx then
               Sets.Get_Socket (Set, K).Send (Data);
            end if;
         end loop;

         Socket.Set_Timeout (10.0);

         declare
            use Ada.Real_Time;
            Stamp : constant Time := Clock;
         begin
            Writer_Task.Delayed_Send (Peer_Idx);

            if Socket.Receive /= Data then
               Put_Line ("Wrond data on timeout test");
            end if;

            if OK_Previous then
               OK_Count := OK_Count - 1;

               if OK_Count <= 0 then
                  Put_Line ("Stop timeout test, enought success");
                  Enought := True;
               end if;

            else
               OK_Previous := True;
            end if;

         exception
            when E : Socket_Error =>
               Writer_Task.Cancel_Delay;

               if Is_Timeout (E) then
                  Put_Line
                    ("Too short timeout" & J'Img
                     & To_Duration (Clock - Stamp)'Img);

                     OK_Previous := False;
                     Error_Count := Error_Count - 1;

                     if Error_Count <= 0 then
                        Put_Line ("Stop timeout test, enought errors");
                        Enought := True;
                     end if;
               else
                  Put_Line (Ada.Exceptions.Exception_Information (E));
                  Enought := True;
               end if;
            when E : others =>
               Put_Line (Ada.Exceptions.Exception_Information (E));
               Enought := True;
         end;

         for K in 1 .. Sets.Count (Set) loop
            --  Read from all sockets except the tested

            if K /= J then
               Read_Data (K, "3");
            end if;
         end loop;

         exit when Enought;

      exception
         when E : others =>
            Put_Line (Ada.Exceptions.Exception_Information (E));
            exit;
      end;
   end loop;

   abort Writer_Task;

   Put_Line ("Timeout test complete");

   --  Test appropriate socket activation

   for J in 1 .. Sets.Count (Set) loop
      Sets.Get_Socket (Set, J - 1 + J rem 2 * 2).Send (Data);

      Sets.Wait (Set, 0.25, Cnt);

      if Cnt /= 1 then
         Put_Line ("Wrong Cnt 1 /=" & Cnt'Img);
      end if;

      if Sets.Is_Read_Ready (Set, J) then
         Read_Data (J, "1");
      else
         Put_Line ("Wrong ready state");
      end if;
   end loop;

   Put_Line ("One socket active test complete");

   --  Random reorder sockets to check poll independence from sockets order

   for J in 1 .. Sets.Count (Set) loop
      declare
         S1, S2 : Socket_Access;
         Idx : constant Sets.Socket_Index :=
           Sets.Socket_Index (AWS.Utils.Random) rem (Sets.Count (Set) / 2 - 1)
           * 2 + 1;
      begin
         Sets.Remove_Socket (Set, Idx, S1);
         Sets.Remove_Socket (Set, Idx + 1, S2);
         Sets.Add (Set, S1, Sets.Input);
         Sets.Add (Set, S2, Sets.Input);
      end;
   end loop;

   for J in reverse 1 .. Sets.Count (Set) / 2 loop
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

      if J rem 2 = 1 then
         Read_Data (J, "2");
      end if;
   end loop;

   Put_Line ("Half sockets active test complete");

exception
   when others =>
      abort Writer_Task;
      raise;
end Max_Poll_Size;
