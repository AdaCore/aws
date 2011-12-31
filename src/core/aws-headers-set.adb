------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO;

with AWS.Containers.Tables.Set;
with AWS.Net.Buffered;

package body AWS.Headers.Set is

   use Ada;
   use AWS.Containers;

   use type Strings.Maps.Character_Set;

   Printable_Set : constant Strings.Maps.Character_Set :=
                     Strings.Maps.To_Set
                       (Strings.Maps.Character_Range'
                          (Low  => ' ',
                           High => Character'Val (126)));
   --  This is RFC2616 CHAR except CTL
   --     CHAR           = <any US-ASCII character (octets 0 - 127)>
   --     CTL            = <any US-ASCII control character
   --                       (octets 0 - 31) and DEL (127)>

   RFC2616_Separator_Set : constant Strings.Maps.Character_Set :=
                             Strings.Maps.To_Set
                               (" ()<>@,;:\""/[]?={}" & Characters.Latin_1.HT);

   RFC2616_Token_Set     : constant Strings.Maps.Character_Set :=
                             Printable_Set - RFC2616_Separator_Set;

   subtype P_List is Tables.Table_Type;

   Debug_Flag : Boolean := False;
   --  Set to True to output debug information to the standard output

   ---------
   -- Add --
   ---------

   procedure Add (Headers : in out List; Name, Value : String) is
   begin
      Tables.Set.Add (P_List (Headers), Name, Value);
   end Add;

   -----------
   -- Debug --
   -----------

   procedure Debug (Activate : Boolean) is
   begin
      Debug_Flag := Activate;
   end Debug;

   ----------
   -- Read --
   ----------

   procedure Read (Socket : Net.Socket_Type'Class; Headers : in out List) is

      procedure Parse_Header_Lines (Line : String);
      --  Parse the Line eventually catenated with the next line if it is a
      --  continuation line see [RFC 2616 - 4.2].

      ------------------------
      -- Parse_Header_Lines --
      ------------------------

      procedure Parse_Header_Lines (Line : String) is
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
                 (Next_Line (Next_Line'First) = ' '
                  or else Next_Line (Next_Line'First) = ASCII.HT)
               then
                  --  Continuing value on the next line. Header fields can be
                  --  extended over multiple lines by preceding each extra
                  --  line with at least one SP or HT.
                  Parse_Header_Lines (Line & Next_Line);

               else
                  if Debug_Flag then
                     Text_IO.Put_Line ('>' & Line);
                  end if;

                  --  Put name and value to the container separately

                  Delimiter_Index := Fixed.Index
                    (Source => Line,
                     Set    => RFC2616_Token_Set,
                     Test   => Outside);

                  if Delimiter_Index = 0                  -- No delimiter
                    or else Delimiter_Index = Line'First  -- Empty name
                    or else Line (Delimiter_Index) /= ':' -- Wrong separator
                  then
                     --  No delimiter, this is not a valid Header Line

                     raise Format_Error with Line;
                  end if;

                  Add (Headers,
                       Name  => Line (Line'First .. Delimiter_Index - 1),
                       Value => Fixed.Trim
                                  (Line (Delimiter_Index + 1 .. Line'Last),
                                   Side => Both));

                  --  Parse next header line

                  Parse_Header_Lines (Next_Line);
               end if;
            end;
         end if;
      end Parse_Header_Lines;

   begin
      Reset (Headers);
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
      Name    : String;
      Value   : String;
      N       : Positive := 1) is
   begin
      Tables.Set.Update (P_List (Headers), Name, Value, N);
   end Update;

end AWS.Headers.Set;
