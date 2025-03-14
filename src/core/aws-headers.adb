------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2024, AdaCore                     --
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
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.Messages;
with AWS.Net.Buffered;

package body AWS.Headers is

   use Ada;
   use Ada.Strings.Unbounded;
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

   Debug_Flag : Boolean := False;
   --  Set to True to output debug information to the standard output

   -----------
   -- Debug --
   -----------

   procedure Debug (Activate : Boolean) is
   begin
      Debug_Flag := Activate;
   end Debug;

   -----------------
   -- Debug_Print --
   -----------------

   procedure Debug_Print (Headers : List) is
   begin
      if Debug_Flag then
         for J in 1 .. Headers.Count loop
            Text_IO.Put_Line ('>' & Headers.Get_Line (J));
         end loop;
      end if;
   end Debug_Print;

   -----------------
   -- Get_Content --
   -----------------

   procedure Get_Content
     (Headers   : List;
      End_Block : Boolean := False) is
   begin
      for J in 1 .. Count (Headers) loop
         Data (Get_Line (Headers, J) & CRLF);
      end loop;

      if End_Block then
         Data (CRLF);
      end if;
   end Get_Content;

   --------------
   -- Get_Line --
   --------------

   function Get_Line (Headers : List; N : Positive) return String is
      Pair : constant Element := Get (Headers, N);
   begin
      if Pair.Name = "" then
         return "";

      elsif Pair.Name = Messages.Get_Token
        or else Pair.Name = Messages.Post_Token
        or else Pair.Name = Messages.Put_Token
        or else Pair.Name = Messages.Head_Token
        or else Pair.Name = Messages.Delete_Token
        or else Pair.Name = Messages.Connect_Token
        or else Pair.Name = HTTP_Version
      then
         --  And header line
         return To_String (Pair.Name & " " & Pair.Value);

      else
         return To_String (Pair.Name & ": " & Pair.Value);
      end if;
   end Get_Line;

   ----------------
   -- Get_Values --
   ----------------

   function Get_Values (Headers : List; Name : String) return String is
      Values : constant VString_Array := Get_Values (Headers, Name);

      function Get_Values (Start_From : Positive) return String;
      --  Return string of header values comma separated
      --  concateneted starting from Start_From index.

      ----------------
      -- Get_Values --
      ----------------

      function Get_Values (Start_From : Positive) return String is
         Value : constant String := To_String (Values (Start_From));
      begin
         if Start_From = Values'Last then
            return Value;
         else
            return Value & ", " & Get_Values (Start_From + 1);
         end if;
      end Get_Values;

   begin
      if Values'Length > 0 then
         return Get_Values (Values'First);
      else
         return "";
      end if;
   end Get_Values;

   ------------
   -- Length --
   ------------

   function Length (Headers : AWS.Headers.List) return Natural is
      L : Natural := 2; -- The ending CR+LF
   begin
      for J in 1 .. AWS.Headers.Count (Headers) loop
         L := @
           + AWS.Headers.Get_Line
               (Headers => Headers,
                N       => J)'Length + 2;
      end loop;

      return L;
   end Length;

   ----------
   -- Read --
   ----------

   procedure Read (Headers : in out List; Socket : Net.Socket_Type'Class) is

      function Get_Line return String;
      --  Read a line from socket

      --------------
      -- Get_Line --
      --------------

      function Get_Line return String is
      begin
         return Net.Buffered.Get_Line (Socket);
      end Get_Line;

      procedure Read is new Read_G (Get_Line);

   begin
      Read (Headers);
   end Read;

   ------------
   -- Read_G --
   ------------

   procedure Read_G (Headers : in out List) is

      procedure Parse_Header_Line (Line : String);
      --  Parse this line, update Headers accordingly

      -----------------------
      -- Parse_Header_Line --
      -----------------------

      procedure Parse_Header_Line (Line : String) is
         use Ada.Strings;
         OWS             : constant Strings.Maps.Character_Set :=
                             Strings.Maps.To_Set (" " & ASCII.HT);
         --  OWS is defined in RFC 9110 section 5.6.3
         Delimiter_Index : Natural;
      begin
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
                       Left  => OWS,
                       Right => OWS));
      end Parse_Header_Line;

      End_Of_Message : constant String := "";
      Line           : Unbounded_String := To_Unbounded_String (Get_Line);

   begin
      Reset (Headers);

      --  Parse the Line eventually catenated with the next line if it is a
      --  continuation line see [RFC 2616 - 4.2].

      loop
         exit when Line = Null_Unbounded_String;

         declare
            Next_Line : constant String := Get_Line;
         begin
            if Next_Line /= End_Of_Message
              and then
                (Next_Line (Next_Line'First) = ' '
                 or else Next_Line (Next_Line'First) = ASCII.HT)
            then
               --  Continuing value on the next line. Header fields can be
               --  extended over multiple lines by preceding each extra
               --  line with at least one SP or HT.

               Append (Line, Next_Line);

            else
               --  Handle current line

               Parse_Header_Line (To_String (Line));

               --  Then start another line with read content

               Line := To_Unbounded_String (Next_Line);
            end if;
         end;
      end loop;
   end Read_G;

   -----------
   -- Reset --
   -----------

   overriding procedure Reset (Headers : in out List) is
   begin
      Tables.Reset (Tables.Table_Type (Headers));
      Headers.Case_Sensitive (False);
   end Reset;

   -----------------
   -- Send_Header --
   -----------------

   procedure Send_Header
     (Socket    : Net.Socket_Type'Class;
      Headers   : List;
      End_Block : Boolean := False)
   is
      procedure Send (Value : String);
      --  Send data over socket

      ----------
      -- Send --
      ----------

      procedure Send (Value : String) is
      begin
         Net.Buffered.Put (Socket, Value);
      end Send;

      procedure Send_Headers_Content is new Get_Content (Send);

   begin
      Send_Headers_Content (Headers, End_Block);

      if End_Block then
         Net.Buffered.Flush (Socket);
      end if;
   end Send_Header;

end AWS.Headers;
