------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2013, AdaCore                     --
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

pragma Ada_2012;

with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO;

with AWS.Utils;

package body AWS.Net.Log.Callbacks is

   procedure Put
     (File        : Text_IO.File_Type;
      Code        : Natural;
      Binary_Mode : Boolean)
     with Inline;
   --  Output character C, if not printable output a single dot

   procedure Put_Hex (File : Text_IO.File_Type; Code : Natural) with Inline;
   --  Output hex code for character C

   procedure Put_Header
     (File      : Text_IO.File_Type;
      Direction : Data_Direction;
      Socket    : Socket_Type'Class;
      Data      : Stream_Element_Array;
      Last      : Stream_Element_Offset);
   --  Output log header into File

   procedure Put_Footer (File : Text_IO.File_Type);
   --  Output log footer into File

   type Counters is array (Data_Direction) of Natural;

   type State is record
      N        : Counters := (others => 0); -- Number of chars read/written
      Log_File : Text_IO.File_Type;
   end record;

   Current_State : State;

   ------------
   -- Binary --
   ------------

   procedure Binary
     (Direction : Data_Direction;
      Socket    : Socket_Type'Class;
      Data      : Stream_Element_Array;
      Last      : Stream_Element_Offset)
   is
      Max_Line : constant := 15;
      F        : Text_IO.File_Type renames Current_State.Log_File;

      procedure Put_Chars
        (Spaces      : Natural;
         First, Last : Stream_Element_Offset);
      --  Output Spaces spaces then the characters from Frist to Last

      ---------------
      -- Put_Chars --
      ---------------

      procedure Put_Chars
        (Spaces      : Natural;
         First, Last : Streams.Stream_Element_Offset)
      is
         use Ada.Strings.Fixed;
      begin
         Text_IO.Put (F, Spaces * " ");
         for K in First .. Last loop
            Put (F, Natural (Data (K)), Binary_Mode => True);
         end loop;
      end Put_Chars;

   begin
      Put_Header (F, Direction, Socket, Data, Last);

      for K in Data'First .. Last loop
         if (K - 1) mod Max_Line = 0 then

            if K /= Data'First then
               --  This is not before the first line, output characters
               Put_Chars (3, K - Max_Line, K);
            end if;

            Text_IO.New_Line (F);
            Text_IO.Put (F, ' ');
            Integer_Text_IO.Put (F, Natural (K), Width => 5);
            Text_IO.Put (F, ": ");
         end if;

         Put_Hex (F, Natural (Data (K)));
         Text_IO.Put (F, ' ');
      end loop;

      --  Output final characters

      declare
         Nb_Last_Line : constant Stream_Element_Offset
           := Last mod Max_Line;
      begin
         Put_Chars
           ((Max_Line - Natural (Nb_Last_Line)) * 3 + 3,
            Last - Nb_Last_Line + 1, Last);
      end;

      Text_IO.New_Line (F, 2);

      Current_State.N (Direction)
        := Current_State.N (Direction) + Natural (Last);

      Put_Footer (F);
      Text_IO.Flush (F);
   end Binary;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
   begin
      Stop;
      Text_IO.Close (Current_State.Log_File);
   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Filename : String;
      Callback : Write_Callback) is
   begin
      Text_IO.Create (Current_State.Log_File, Text_IO.Out_File, Filename);
      Start (Callback);
   end Initialize;

   ---------
   -- Put --
   ---------

   procedure Put
     (File        : Text_IO.File_Type;
      Code        : Natural;
      Binary_Mode : Boolean)
   is
      C : constant Character := Character'Val (Code);
   begin
      if Strings.Maps.Is_In (C, Strings.Maps.Constants.Graphic_Set)
        or else (not Binary_Mode and then (C = ASCII.CR or else C = ASCII.LF))
      then
         Text_IO.Put (File, C);
      else
         Text_IO.Put (File, '.');
      end if;
   end Put;

   ----------------
   -- Put_Footer --
   ----------------

   procedure Put_Footer (File : Text_IO.File_Type) is
   begin
      Text_IO.Put_Line
        (File, "     Total data sent: " & Utils.Image (Current_State.N (Sent))
         & " received: " & Utils.Image (Current_State.N (Received)));
      Text_IO.New_Line (File);
   end Put_Footer;

   ----------------
   -- Put_Header --
   ----------------

   procedure Put_Header
     (File      : Text_IO.File_Type;
      Direction : Data_Direction;
      Socket    : Socket_Type'Class;
      Data      : Stream_Element_Array;
      Last      : Stream_Element_Offset) is
   begin
      Text_IO.Put (File, "Data ");

      case Direction is
         when Sent     => Text_IO.Put (File, "sent to ");
         when Received => Text_IO.Put (File, "received from ");
      end case;

      Text_IO.Put (File, "socket " & Utils.Image (Get_FD (Socket)));
      Text_IO.Put_Line
        (File, " (" & Utils.Image (Natural (Last))
         & "/" & Utils.Image (Natural (Data'Last)) & ')');
   end Put_Header;

   -------------
   -- Put_Hex --
   -------------

   procedure Put_Hex (File : Text_IO.File_Type; Code : Natural) is
   begin
      Text_IO.Put (File, Utils.Hex (Code, Width => 2));
   end Put_Hex;

   ----------
   -- Text --
   ----------

   procedure Text
     (Direction : Data_Direction;
      Socket    : Socket_Type'Class;
      Data      : Stream_Element_Array;
      Last      : Stream_Element_Offset)
   is
      Max_Line : constant := 70;
      LF       : constant Stream_Element
        := Stream_Element (Character'Pos (ASCII.LF));
      F        : Text_IO.File_Type renames Current_State.Log_File;
      C        : Natural := 0;
   begin
      Put_Header (F, Direction, Socket, Data, Last);

      for K in Data'First .. Last loop
         if C mod Max_Line = 0 or else Data (K) = LF then
            Text_IO.New_Line (F);
            Text_IO.Put (F, ' ');
            Integer_Text_IO.Put (F, Natural (K), Width => 5);
            Text_IO.Put (F, ": ");
            C := 0;
         end if;

         C := C + 1;
         if Data (K) /= LF then
            Put (F, Natural (Data (K)), Binary_Mode => False);
         end if;
      end loop;

      Text_IO.New_Line (F, 2);

      Current_State.N (Direction)
        := Current_State.N (Direction) + Natural (Last);

      Put_Footer (F);
      Text_IO.Flush (F);
   end Text;

end AWS.Net.Log.Callbacks;
