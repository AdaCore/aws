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

with Ada.Exceptions;
with Ada.Streams;
with Ada.Text_IO;

with AWS.Net.Log;
with AWS.Net.SSL;
with AWS.Utils;

with GNAT.MD5;

procedure SNB_Send is
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

   Socket_Timeout : constant Duration := 0.25;
   Timeout_Image  : constant String :=
     AWS.Utils.Significant_Image (Socket_Timeout, 2);

   -----------
   -- Error --
   -----------

   procedure Error (Socket : Socket_Type'Class; Message : String) is
      Last : constant Positive := Message'Last -
        (if Message'Length > Timeout_Image'Length
           and then Message
                      (Message'Last - Timeout_Image'Length .. Message'Last)
                    = ' ' & Timeout_Image
         then Timeout_Image'Length
         else 0);
   begin
      Put_Line ("error: " & Message (Message'First .. Last));
   end Error;

begin
   Log.Start (Write => null, Error => SNB_Send.Error'Unrestricted_Access);

   for J in Data'Range loop
      Data (J) := Ada.Streams.Stream_Element'Mod (J);
   end loop;

   for J in S'Range loop
      S (J) := Socket (False);
   end loop;

   Socket_Pair (S (S'Last).all, S (S'First).all);

   for J in S'Range loop
      S (J).Set_Send_Buffer_Size (70_000);
      S (J).Set_Receive_Buffer_Size (50_000);
   end loop;

   for J in S'Range loop
      S (J).Send (Data, Last);
      GNAT.MD5.Update (MD5W, (Data (1 .. Last)));
      --  Put_Line ("Send" & Last'Img);
      while S (not J).Pending > 0 loop
         S (not J).Receive (Data, Last);
         GNAT.MD5.Update (MD5R, (Data (1 .. Last)));
         --  Put_Line ("Receive" & Last'Img);
      end loop;

      S (J).Set_Timeout (Socket_Timeout);
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
      declare
         Sent : Ada.Streams.Stream_Element_Offset := 0;
         Got  : Ada.Streams.Stream_Element_Offset := 0;
         Stop : Boolean := False;

         task Receive is
            entry Done (Complete : Boolean);
         end Receive;

         task body Receive is
            Data : Ada.Streams.Stream_Element_Array (1 .. 8000);
            Last : Ada.Streams.Stream_Element_Offset;
            Wait : Duration := Duration'Small;
         begin
            loop
               begin
                  S (not J).Receive (Data, Last);
                  GNAT.MD5.Update (MD5R, (Data (1 .. Last)));
                  Got := Got + Last;

                  if not Stop then
                     delay Wait;
                     Wait := Wait * 2;
                  end if;

                  --  Put (Last'Img & S (J).Output_Space'Img & 'r');

               exception when E : Socket_Error =>
                  if Is_Timeout (E) then
                     Stop := False;

                     if GNAT.MD5.Digest (MD5W) /= GNAT.MD5.Digest (MD5R) then
                        Put_Line (GNAT.MD5.Digest (MD5W));
                        Put_Line (GNAT.MD5.Digest (MD5R));
                     end if;

                     if Sent /= Got then
                        Put_Line ("Send /= Got" & Sent'Img & Got'Img);
                     end if;

                     accept Done (Complete : Boolean) do
                        if Complete then
                           Wait := 0.0;
                        else
                           Wait := Duration'Small;
                        end if;
                     end Done;

                  else
                     Put_Line
                       ("Receive " & Ada.Exceptions.Exception_Information (E));
                     exit;
                  end if;
               end;

               exit when Wait = 0.0;
            end loop;

            Put_Line ("Complete");

         exception when E : others =>
            Put_Line
              ("task Receive " & Ada.Exceptions.Exception_Information (E));
         end Receive;

         subtype Block_Sizes is
           Ada.Streams.Stream_Element_Offset range 16_200 .. 16_300;
         BL : Block_Sizes := Block_Sizes'Last;

      begin
         S (J).Send (Data, Last);
         GNAT.MD5.Update (MD5W, (Data (1 .. Last)));
         Sent := Sent + Last;

         loop
            S (J).Send (Data (1 .. BL + Boolean'Pos (J) * 100), Last);

            if Last = 0 then
               Stop := True;
               Put_Line (BL'Img & " sent");

               BL := BL - 1;

               if BL = Block_Sizes'First then
                  Receive.Done (True);
                  exit;
               else
                  Receive.Done (False);
               end if;
            else
               GNAT.MD5.Update (MD5W, (Data (1 .. Last)));
               Sent := Sent + Last;
            end if;

            --  Put (Last'Img & S (J).Output_Space'Img & ';');
         end loop;

         if GNAT.MD5.Digest (MD5W) /= GNAT.MD5.Digest (MD5R)
           or else Sent /= Got
         then
            Put_Line ("Sent" & Sent'Img & ' ' & GNAT.MD5.Digest (MD5W));
            Put_Line ("Got " & Got'Img  & ' ' & GNAT.MD5.Digest (MD5R));
            Put_Line ("Send and read digest differ.");
         end if;

      exception when E : others =>
         Put_Line ("main Send " & Ada.Exceptions.Exception_Information (E));
         S (J).Shutdown;
         Receive.Done (True);
         exit;
      end;
   end loop;
end SNB_Send;
