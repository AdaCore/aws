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

with AWS.Net.SSL;
with Ada.Exceptions;
with Ada.Streams;
with Ada.Text_IO;

with GNAT.MD5;

procedure Big_Send is
   use AWS.Net;
   use Ada.Text_IO;
   use type Ada.Streams.Stream_Element_Offset;
   use type Ada.Streams.Stream_Element;
   S : array (Boolean) of Socket_Access;
   Data : Ada.Streams.Stream_Element_Array (1 .. 16#10000#);
   Last : Ada.Streams.Stream_Element_Offset;
   Conf : SSL.Config;
   MD5R : GNAT.MD5.Context;
   MD5W : GNAT.MD5.Context;

begin
   for J in Data'Range loop
      Data (J) := Ada.Streams.Stream_Element'Mod (J);
   end loop;

   for J in S'Range loop
      S (J) := Socket (False);
   end loop;

   Socket_Pair (S (S'Last).all, S (S'First).all);

   for J in S'Range loop
      Put_Line
        (S (J).Get_Send_Buffer_Size'Img & S (J).Get_Receive_Buffer_Size'Img);

      S (J).Set_Send_Buffer_Size (70_000);
      S (J).Set_Receive_Buffer_Size (50_000);
   end loop;

   Put_Line (SSL.Version & Data'Last'Img);

   for J in S'Range loop
      S (J).Send (Data, Last);
      GNAT.MD5.Update (MD5W, (Data (1 .. Last)));
      Put_Line ("Send" & Last'Img);
      while S (not J).Pending > 0 loop
         S (not J).Receive (Data, Last);
         GNAT.MD5.Update (MD5R, (Data (1 .. Last)));
         Put_Line ("Receive" & Last'Img);
      end loop;
   end loop;

   SSL.Initialize (Conf, "cert.pem");

   S (S'First) := new Socket_Type'Class'
                        (Socket_Type'Class
                           (SSL.Secure_Client (S (S'First).all, Conf)));
   S (S'Last)  := new Socket_Type'Class'
                        (Socket_Type'Class
                           (SSL.Secure_Server (S (S'Last).all, Conf)));
   Put_Line ("--------------------------------------------------------------");

   for J in S'Range loop
      S (not J).Set_Timeout (2.0);

      declare
         Sent : Ada.Streams.Stream_Element_Offset := 0;
         Got  : Ada.Streams.Stream_Element_Offset := 0;
         Stop : Boolean := False;

         task Receive is
            entry Done;
         end Receive;

         task body Receive is
            Data : Ada.Streams.Stream_Element_Array (1 .. 8000);
            Last : Ada.Streams.Stream_Element_Offset;
         begin
            loop
               begin
                  S (not J).Receive (Data, Last);
                  GNAT.MD5.Update (MD5R, (Data (1 .. Last)));
               exception when E : Socket_Error =>
                  exit when Is_Timeout (E);
                  Put_Line (Ada.Exceptions.Exception_Information (E));
                  exit;
               end;

               if not Stop then
                  delay 0.0;
               end if;

               Got := Got + Last;
            end loop;

            accept Done;
         end Receive;

      begin
         loop
            S (J).Send (Data, Last);
            GNAT.MD5.Update (MD5W, (Data (1 .. Last)));
            exit when Last = 0;
            Sent := Sent + Last;
            Put (Last'Img & S (J).Output_Space'Img & ';');
         end loop;
         New_Line;

         Stop := True;
         Receive.Done;

         Put_Line ("Sent" & Sent'Img);
         Put_Line ("Got " & Got'Img);
      end;
   end loop;

   Put_Line (GNAT.MD5.Digest (MD5R));
   Put_Line (GNAT.MD5.Digest (MD5W));
end Big_Send;
