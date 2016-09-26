------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                        Copyright (C) 2016, AdaCore                       --
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

with Ada.Characters.Handling;
with Ada.Streams;         use Ada.Streams;
with Ada.Text_IO;         use Ada.Text_IO;
with AWS.Net.Poll_Events; use AWS.Net;
with AWS.Net.Std;
with AWS.OS_Lib;
with AWS.Utils;
with Interfaces.C.Strings;

procedure Poll is
   Set : Poll_Events.Set (45); -- Have to be odd
   Ss  : array (1 .. Set.Size) of Std.Socket_Type;
   Local : constant String := Localhost (IPv6_Available);
   Count : Integer;
   Idx   : Positive;
   State : Event_Set;
   Data  : Stream_Element_Array (1 .. 32);
   Last  : Stream_Element_Offset;
   More  : constant Stream_Element_Offset := 8;
   Rest  : Natural := 0;

   function Error_Message (Errno : Integer) return String;

   function Image_State return String;
   --  Returns image of State array in format like eIo or eiO or Eio

   -------------------
   -- Error_Message --
   -------------------

   function Error_Message (Errno : Integer) return String is
      use AWS;
      use Interfaces.C.Strings;
      Ptr  : constant chars_ptr := OS_Lib.Socket_StrError (Errno);
      Code : constant String    := '[' & Utils.Image (Errno) & "] ";
   begin
      if Ptr = Null_Ptr then
         return Code & "Unknown error";
      else
         return Code & Value (Ptr);
      end if;
   end Error_Message;

   -----------------
   -- Image_State --
   -----------------

   function Image_State return String is
      Result : String (1 .. 3);
      Idx : Positive := Result'First;
   begin
      for J in State'Range loop
         Result (Idx) := J'Img (1);

         if not State (J) then
            Result (Idx) := Ada.Characters.Handling.To_Lower (Result (Idx));
         end if;

         Idx := Idx + 1;
      end loop;

      return Result;
   end Image_State;

begin
   Ss (1).Bind (Host => Local, Port => 0);
   Ss (1).Listen (Queue_Size => Set.Size);
   Set.Add (Ss (1).Get_FD, (Input => True, others => False));

   ------------------
   -- Connect loop --
   ------------------

   for J in 2 .. Set.Size / 2 + 1 loop
      Ss (J).Connect (Local, Port => Ss (1).Get_Port, Wait => False);
      Set.Add (Ss (J).Get_FD, (Output => True, others => False));
   end loop;

   Put_Line ("Connection");

   -----------------
   -- Accept loop --
   -----------------

   loop
      Set.Wait (0.5, Count);

      Idx := 1;

      while Count > 0 loop
         Set.Next (Idx);
         exit when Idx > Set.Length;
         Count := Count - 1;

         State := Set.Status (Idx);

         if State (Error) then
            Put_Line
              ("Unexpected error on accept " & Error_Message (Ss (Idx).Errno)
               & Idx'Img);
            return;

         elsif State (Input) and then State (Output) then
            Put_Line ("Unexpected state on accept " & Image_State & Idx'Img);
            return;

         elsif State (Output) then
            Set.Set_Mode (Idx, (Input => True, others => False));

         elsif State (Input) then
            if Idx /= 1 then
               Put_Line ("Unexpected input index" & Idx'Img);
            end if;

            Std.Accept_Socket (Ss (1), Ss (Set.Length + 1));
            Set.Add
              (Ss (Set.Length + 1).Get_FD, (Input => True, others => False));
         end if;

         Idx := Idx + 1;
      end loop;

      if Count /= 0 then
         Put_Line ("Wrong sockets event count " & Count'Img);
         exit;
      end if;

      exit when Set.Length = Ss'Length;
   end loop;

   Put_Line ("Connected");

   ----------------------------------
   -- Write over whole sockets set --
   ----------------------------------

   for J in 2 .. Ss'Last loop
      Ss (J).Send ((1 => Stream_Element (J)));

      Set.Wait (0.5, Count);

      if Count /= 1 then
         Put_Line ("Unexpected number of activated sockets" & Count'Img);
      end if;

      Idx := 1;
      Set.Next (Idx);

      State := Set.Status (Idx);

      if State /= (Input => True, Output => False, Error => False) then
         Put_Line ("Unexpected state on send loop " & Image_State);

         if State (Error) then
            Put_Line
              ("Unexpected error on send loop "
               & Error_Message (Ss (Idx).Errno) & Idx'Img);
         end if;

         return;
      end if;

      Ss (Idx).Receive (Data, Last);

      if Last /= Data'First then
         Put_Line ("Unexpected received data length" & Last'Img);
      end if;

      if Integer (Data (Data'First)) /= J then
         Put_Line ("Unexpected received content");
      end if;
   end loop;

   Put_Line ("Transmitted");

   for J in 2 .. Ss'Last loop
      Ss (J).Send ((1 .. Data'Length + More => Stream_Element (J)));
      Ss (J).Set_Timeout (2.0);
   end loop;

   -------------------------------------------
   -- Test waiting neither Input nor Output --
   -------------------------------------------

   loop
      Set.Wait (0.25, Count);

      exit when Count = 0;

      Idx := 1;

      while Count > 0 loop
         Set.Next (Idx);
         exit when Idx > Set.Length;
         Count := Count - 1;

         State := Set.Status (Idx);

         if State /= (Input => True, Output => False, Error => False) then
            Put_Line ("Unexpected state on test " & Image_State);

            if State (Error) then
               Put_Line
                 ("Unexpected error on test " & Error_Message (Ss (Idx).Errno)
                  & Idx'Img);
            end if;

            return;
         end if;

         Ss (Idx).Receive (Data, Last);

         if Last = Data'Last then
            Put ('@');

            if Idx rem 2 = 0 then
               Set.Set_Event (Idx, Input, False);
            end if;

         elsif Last = More then
            Rest := Rest + 1;
         else
            Put (" Wrong additional size" & Last'Img);
            exit;
         end if;

         Idx := Idx + 1;
      end loop;

      if Count /= 0 then
         Put ("Wrong rest count" & Count'Img);
         exit;
      end if;
   end loop;

   Put_Line (Rest'Img);
end Poll;
