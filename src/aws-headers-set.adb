------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                                ACT-Europe                                --
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

with Ada.Strings.Fixed;
with Ada.Text_IO;
with AWS.Containers.Tables.Set;
with AWS.Net.Buffered;

package body AWS.Headers.Set is

   use AWS.Containers;

   subtype P_List is Tables.Table_Type;

   Debug_Flag : Boolean := False;
   --  Set to True to output debug information to the standard output.

   ---------
   -- Add --
   ---------

   procedure Add (Headers : in out List; Name, Value : in String) is
   begin
      Tables.Set.Add (P_List (Headers), Name, Value);
   end Add;

   -----------
   -- Debug --
   -----------

   procedure Debug (Activate : in Boolean) is
   begin
      Debug_Flag := Activate;
   end Debug;

   ----------
   -- Free --
   ----------

   procedure Free (Headers : in out List) is
   begin
      Tables.Set.Free (P_List (Headers));
   end Free;

   ----------
   -- Read --
   ----------

   procedure Read (Headers : in out List; Socket : in Net.Socket_Type'Class)
   is

      procedure Parse_Header_Lines (Line : in String);
      --  Parse the Line eventually catenated with the next line if it is a
      --  continuation line see [RFC 2616 - 4.2].

      ------------------------
      -- Parse_Header_Lines --
      ------------------------

      procedure Parse_Header_Lines (Line : in String) is
         End_Of_Message : constant String := "";
      begin
         if Line = End_Of_Message then
            return;
         else
            declare
               use Ada.Strings;

               Next_Line       : constant String :=
                  Net.Buffered.Get_Line (Socket);
               Delimiter_Index : Natural;

            begin
               if Next_Line /= End_Of_Message
                    and then
                 (Next_Line (1) = ' ' or else Next_Line (1) = ASCII.HT)
               then
                  --  Continuing value on the next line. Header fields can be
                  --  extended over multiple lines by preceding each extra
                  --  line with at least one SP or HT.
                  Parse_Header_Lines (Line & Next_Line);

               else
                  if Debug_Flag then
                     Ada.Text_IO.Put_Line ('>' & Line);
                  end if;

                  --  Put name and value to the container separately.

                  Delimiter_Index := Fixed.Index (Line, ":");

                  if Delimiter_Index = 0 then
                     --  No delimiter, this is not a valid Header Line
                     raise Format_Error;
                  end if;

                  Add (Headers,
                       Name  => Line (Line'First .. Delimiter_Index - 1),
                       Value => Fixed.Trim
                                  (Line (Delimiter_Index + 1 .. Line'Last),
                                   Side => Both));

                  --  Parse next header line.

                  Parse_Header_Lines (Next_Line);
               end if;
            end;
         end if;
      end Parse_Header_Lines;

   begin
      Parse_Header_Lines (Net.Buffered.Get_Line (Socket));
   end Read;

   -----------
   -- Reset --
   -----------

   procedure Reset (Headers : in out List) is
   begin
      Tables.Set.Reset (P_List (Headers));
      Tables.Set.Case_Sensitive (P_List (Headers), False);
   end Reset;

   ------------
   -- Update --
   ------------

   procedure Update
     (Headers : in out List;
      Name    : in     String;
      Value   : in     String;
      N       : in     Positive := 1) is
   begin
      Tables.Set.Update (P_List (Headers), Name, Value, N);
   end Update;

end AWS.Headers.Set;
