------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2002-2014, AdaCore                     --
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

with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;

with AWS.Utils;

package body AWS.Headers.Values is

   use Ada.Strings;
   use type Maps.Character_Set;

   EDel : constant Maps.Character_Set := Utils.Spaces or Maps.To_Set (",;");
   --  Delimiter between name/value pairs in the HTTP header lines.
   --  In WWW-Authenticate, header delimiter between name="Value" pairs is a
   --  comma.
   --  In the Set-Cookie header, value delimiter between name="Value" pairs is
   --  a semi-colon.

   procedure Next_Value
     (Data        : String;
      First       : in out Natural;
      Name_First  : out Positive;
      Name_Last   : out Natural;
      Value_First : out Positive;
      Value_Last  : out Natural);
   --  Returns the next named or un-named value from Data. It start the search
   --  from First index. Returns First = 0 if it has reached the end of
   --  Data. Returns Name_Last = 0 if an un-named value has been found.

   -----------------------
   -- Get_Unnamed_Value --
   -----------------------

   function Get_Unnamed_Value
     (Header_Value : String; N : Positive := 1) return String
   is
      First       : Natural;
      Name_First  : Positive;
      Name_Last   : Natural;
      Value_First : Positive;
      Value_Last  : Natural;

      Count       : Natural := 0;

   begin
      First := Fixed.Index
                 (Source => Header_Value,
                  Set    => EDel,
                  Test   => Outside);

      if First = 0 then
         --  Value is empty or contains only spaces
         return "";
      end if;

      loop
         Next_Value
           (Header_Value, First,
            Name_First, Name_Last,
            Value_First, Value_Last);

         if Name_Last = 0 then
            Count := Count + 1;

            if Count = N then
               return Header_Value (Value_First .. Value_Last);
            end if;
         end if;

         exit when First = 0;
      end loop;

      --  There is not such value, return the empty string
      return "";
   end Get_Unnamed_Value;

   ------------
   --  Index --
   ------------

   function Index
     (Set            : Values.Set;
      Name           : String;
      Case_Sensitive : Boolean := True) return Natural
   is
      Map    : Maps.Character_Mapping;
      M_Name : Unbounded_String;
   begin
      if Case_Sensitive then
         Map    := Maps.Identity;
         M_Name := To_Unbounded_String (Name);
      else
         Map    := Maps.Constants.Upper_Case_Map;
         M_Name := Translate (To_Unbounded_String (Name), Map);
      end if;

      for I in Set'Range loop
         if Set (I).Named_Value
           and then Translate (Set (I).Name, Map) = M_Name
         then
            return I;
         end if;
      end loop;

      --  Name was not found, return 0
      return 0;
   end Index;

   ----------------
   -- Next_Value --
   ----------------

   procedure Next_Value
     (Data        : String;
      First       : in out Natural;
      Name_First  : out Positive;
      Name_Last   : out Natural;
      Value_First : out Positive;
      Value_Last  : out Natural)
   is
      NVDel : constant Character := '=';
      --  Delimiter between name and Value for a named value

      VDel  : constant Maps.Character_Set := EDel or Maps.To_Set (NVDel);
      --  Delimiter between name and value is '=' and it is a space between
      --  un-named values.

      Last  : Natural;

   begin
      Last := Fixed.Index (Data (First .. Data'Last), VDel);

      Name_Last := 0;
      Name_First := Positive'Last;

      if Last = 0 then
         --  This is the last single value

         Value_First := First;
         Value_Last  := Data'Last;
         First       := 0; -- Mean end of line

      elsif Data (Last) = '=' then
         --  Here we have a named value

         Name_First := First;
         Name_Last  := Last - 1;
         First      := Last + 1;

         --  Check if this is a quoted or unquoted value

         if First < Data'Last and then Data (First) = '"' then
            --  Quoted value

            Value_First := First + 1;

            Last := Fixed.Index (Data (Value_First .. Data'Last), """");

            if Last = 0 then
               --  Format error as there is no closing quote

               raise Format_Error
                 with "HTTP header line format error : " & Data;
            else
               Value_Last := Last - 1;
            end if;

            First := Last + 2;

         else
            --  Unquoted value

            Value_First := First;

            Last := Ada.Strings.Fixed.Index (Data (First .. Data'Last), EDel);

            if Last = 0 then
               Value_Last := Data'Last;
               First      := 0;
            else
               Value_Last := Last - 1;
               First      := Last + 1;
            end if;
         end if;
      else
         --  This is an un-named value

         Value_First := First;
         Value_Last  := Last - 1;
         First       := Last + 1;

         --  Do not return the delimiter as part of the value

         while Maps.Is_In (Data (Value_Last), EDel) loop
            Value_Last := Value_Last - 1;
         end loop;

      end if;

      if First > Data'Last then
         --  We have reached the end-of-line
         First := 0;

      elsif First > 0 then
         --  Ignore the next leading spaces

         First := Fixed.Index
            (Source => Data (First .. Data'Last),
             Set    => Utils.Spaces,
             Test   => Outside);
      end if;
   end Next_Value;

   -----------
   -- Parse --
   -----------

   procedure Parse (Header_Value : String) is

      First       : Natural;
      Name_First  : Positive;
      Name_Last   : Natural;
      Value_First : Positive;
      Value_Last  : Natural;
      Quit        : Boolean;

   begin
      --  Ignore the leading spaces

      First := Fixed.Index
                 (Source => Header_Value,
                  Set    => EDel,
                  Test   => Outside);

      if First = 0 then
         --  Value is empty or contains only spaces
         return;
      end if;

      loop
         Next_Value
           (Header_Value, First,
            Name_First,  Name_Last,
            Value_First, Value_Last);

         Quit := False;

         if Name_Last > 0 then
            Named_Value
              (Header_Value (Name_First .. Name_Last),
               Header_Value (Value_First .. Value_Last),
               Quit);
         else
            Value (Header_Value (Value_First .. Value_Last), Quit);
         end if;

         exit when Quit or else First = 0;
      end loop;
   end Parse;

   ------------
   -- Search --
   ------------

   function Search
     (Header_Value   : String;
      Name           : String;
      Case_Sensitive : Boolean := True) return String
   is
      First       : Natural;
      Name_First  : Positive;
      Name_Last   : Natural;
      Value_First : Positive;
      Value_Last  : Natural;

      Map         : Maps.Character_Mapping;

      M_Name      : String (Name'Range);
      --  Mapped name

   begin
      First := Fixed.Index
                 (Source => Header_Value,
                  Set    => EDel,
                  Test   => Outside);

      if First = 0 then
         --  Value is empty or contains only spaces
         return "";
      end if;

      if Case_Sensitive then
         Map    := Maps.Identity;
         M_Name := Name;
      else
         Map    := Maps.Constants.Upper_Case_Map;
         M_Name := Fixed.Translate (Name, Map);
      end if;

      loop
         Next_Value
           (Header_Value, First,
            Name_First, Name_Last,
            Value_First, Value_Last);

         if Name_Last > 0
              and then
           M_Name =
             Fixed.Translate (Header_Value (Name_First .. Name_Last), Map)
         then
            return Header_Value (Value_First .. Value_Last);
         end if;

         exit when First = 0;
      end loop;

      --  Name not found, returns the empty string
      return "";
   end Search;

   -----------
   -- Split --
   -----------

   function Split (Header_Value : String) return Set is

      First    : Natural;
      Null_Set : Set (1 .. 0);

      function To_Set return Set;
      --  Parse the Header_Value and return a set of named and un-named
      --  value. Note that this routine is recursive as the final Set size is
      --  not known. This should not be a problem as the number of token on an
      --  Header_Line is quite small.

      ------------
      -- To_Set --
      ------------

      function To_Set return Set is

         Name_First  : Positive;
         Name_Last   : Natural;
         Value_First : Positive;
         Value_Last  : Natural;

         function Element return Data;
         --  Returns the Data element from the substrings defined by
         --  Name_First, Name_Last, Value_First, Value_Last.

         -------------
         -- Element --
         -------------

         function Element return Data is
            function "+"
              (Item : String)
               return Unbounded_String
               renames To_Unbounded_String;
         begin
            if Name_Last = 0 then
               return Data'
                 (Named_Value => False,
                  Value => +Header_Value (Value_First .. Value_Last));
            else
               return Data'
                  (True,
                   Name  => +Header_Value (Name_First .. Name_Last),
                   Value => +Header_Value (Value_First .. Value_Last));
            end if;
         end Element;

      begin
         if First = 0 then
            --  This is the end of recursion
            return Null_Set;
         end if;

         Next_Value
           (Header_Value, First,
            Name_First, Name_Last,
            Value_First, Value_Last);

         return Element & To_Set;
      end To_Set;

   begin
      First := Fixed.Index
        (Source => Header_Value,
         Set    => EDel,
         Test   => Outside);

      return To_Set;
   end Split;

   --------------------------
   -- Unnamed_Value_Exists --
   --------------------------

   function Unnamed_Value_Exists
     (Header_Value   : String;
      Value          : String;
      Case_Sensitive : Boolean := True) return Boolean
   is
      First       : Natural;
      Name_First  : Positive;
      Name_Last   : Natural;
      Value_First : Positive;
      Value_Last  : Natural;

      Map     : Maps.Character_Mapping;
      M_Value : String (Value'Range);

   begin
      First := Fixed.Index
                 (Source => Header_Value,
                  Set    => EDel,
                  Test   => Outside);

      if First = 0 then
         --  Value is empty or contains only spaces
         return False;
      end if;

      if Case_Sensitive then
         Map     := Maps.Identity;
         M_Value := Value;
      else
         Map     := Maps.Constants.Upper_Case_Map;
         M_Value := Fixed.Translate (Value, Map);
      end if;

      loop
         Next_Value
           (Header_Value, First,
            Name_First, Name_Last,
            Value_First, Value_Last);

         if Name_Last = 0
           and then M_Value = Fixed.Translate
              (Header_Value (Value_First .. Value_Last), Map)
         then
            return True;
         end if;

         exit when First = 0;
      end loop;

      --  There is not such value
      return False;
   end Unnamed_Value_Exists;

end AWS.Headers.Values;
