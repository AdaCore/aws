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

with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;

package body AWS.Headers.Values is

   use Ada.Strings;

   Spaces : constant Maps.Character_Set := Maps.To_Set
     (' ' & ASCII.HT & ASCII.LF & ASCII.CR);

   procedure Parsing_Step
      (Data        : in     String;
       First       : in out Natural;
       Name_First  :    out Positive;
       Name_Last   :    out Natural;
       Value_First :    out Positive;
       Value_Last  :    out Natural);
   --  Routine finding the next Name/Value of single value
   --  and moving First to the next place.
   --  if the First is 0 then the end of line reached.
   --  if the Name_Last = 0 then found a value without name.

   ------------
   --  Index --
   ------------

   function Index
     (S              : in Set;
      Name           : in String;
      Case_Sensitive : in Boolean := True) return Natural
   is
      Map    : Maps.Character_Mapping;
      Sample : Unbounded_String;
   begin

      if Case_Sensitive then
         Map := Maps.Identity;
      else
         Map := Maps.Constants.Upper_Case_Map;
      end if;

      Sample := Translate (To_Unbounded_String (Name), Map);

      for I in S'Range loop
         if S (I).Named_Value
           and then Translate (S (I).Name, Map) = Sample
         then
            return I;
         end if;
      end loop;
      return 0;
   end Index;

   -----------
   -- Parse --
   -----------

   procedure Parse (Value : in String) is

      First       : Natural;
      Name_First  : Positive;
      Name_Last   : Natural;
      Value_First : Positive;
      Value_Last  : Natural;
      Quit        : Boolean;

   begin

      --  ignoring the leading spaces.
      First := Fixed.Index
            (Source => Value,
             Set    => Spaces,
             Test   => Outside);

      if First = 0 then
         return;
      end if;

      loop

         Parsing_Step (Value, First,
            Name_First,  Name_Last,
            Value_First, Value_Last);

         Quit := False;

         if Name_Last > 0 then
            Named_Value
              (Value (Name_First .. Name_Last),
               Value (Value_First .. Value_Last),
               Quit);
         else
            Alone_Value
              (Value (Value_First .. Value_Last),
               Quit);
         end if;

         exit when Quit or First = 0;

      end loop;

   end Parse;

   ------------------
   -- Parsing_Step --
   ------------------

   procedure Parsing_Step
     (Data        : in     String;
      First       : in out Natural;
      Name_First  :    out Positive;
      Name_Last   :    out Natural;
      Value_First :    out Positive;
      Value_Last  :    out Natural)
   is

      EDel   : constant Maps.Character_Set := Maps.To_Set (",;");
      --  Delimiter between name/value pairs in the http header line
      --  In WWW-Authenticate header delimiter between name="Value"
      --  pairs is a comma.
      --  In the Set-Cookie header value delimiter between name="Value"
      --  pairs is a semi-colon.

      UVDel  : constant Character := ' ';
      --  right delimiter of the unnamed value.
      NVDel  : constant Character := '=';
      --  Delimiter between name and Value

      Spaces : constant Maps.Character_Set := Maps.To_Set
        (' ' & ASCII.HT & ASCII.LF & ASCII.CR);

      VDel   : constant Maps.Character_Set := Maps.To_Set (UVDel & NVDel);
      --  Delimiter between name and value is '='
      --  or single value delimiter is space.

      Last        : Natural;
   begin

      Last := Fixed.Index (Data (First .. Data'Last), VDel);

      Name_Last := 0;

      --  If last single value.
      if Last = 0 then

         Value_First := First;
         Value_Last  := Data'Last;
         First       := 0; -- mean end of line.

      --  If single value.
      elsif Data (Last) = UVDel then
         Value_First := First;
         Value_Last  := Last - 1;
         First       := Last + 1;

      else
         Name_First := First;
         Name_Last  := Last - 1;
         First      := Last + 1;

         --  Quoted value

         if Data (First) = '"' then
            Value_First := First + 1;

            Last := Fixed.Index
              (Data (Value_First .. Data'Last), """");

            --  If format error

            if Last = 0 then
               Ada.Exceptions.Raise_Exception
                 (Format_Error'Identity,
                  "HTTP header line format error: " & Data);
            else
               Value_Last := Last - 1;
            end if;

            First := Last + 2;

         else
            --  Unquoted value

            Value_First := First;

            Last := Ada.Strings.Fixed.Index
              (Data (First .. Data'Last), EDel);

            if Last = 0 then
               Value_Last := Data'Last;
               First      := 0;
            else
               Value_Last := Last - 1;
               First      := Last + 1;
            end if;

         end if;

      end if;

      if First > Data'Last then
         First := 0;
      elsif First > 0 then
         --  ignoring the next leading spaces.
         First := Fixed.Index
            (Source => Data (First .. Data'Last),
             Set    => Spaces,
             Test   => Outside);
      end if;

   end Parsing_Step;

   -----------
   -- Split --
   -----------

   function Split (Value : in String) return Set is

      First  : Natural;
      Empty       : Set (1 .. 0);

      function To_Set return Set;
      --  Converting Data (Data_First .. Data'Last) to the Set.
      --  We have to use recursive routine, becouse we could not
      --  declare the array, and then fill it.
      --  The constraint error going to occure this way.

      function To_Set return Set is

         Name_First  : Positive;
         Name_Last   : Natural;
         Value_First : Positive;
         Value_Last  : Natural;

         function Element return Data;
         --  Create the Data element from the substrings defined by
         --  Name_First, Name_Last, Value_First, Value_Last.

         -------------
         -- Element --
         -------------

         function Element return Data
         is
            function V (Item : String) return Unbounded_String
               renames To_Unbounded_String;
         begin
            if Name_Last = 0 then
               return Data'
                 (Named_Value => False,
                  Value => V (Value (Value_First .. Value_Last)));
            else
               return Data'
                  (True,
                   Name  => V (Value (Name_First .. Name_Last)),
                   Value => V (Value (Value_First .. Value_Last)));
            end if;
         end Element;

      begin
         if First = 0 then
            return Empty;
         end if;

         Parsing_Step (Value, First,
            Name_First,  Name_Last,
            Value_First, Value_Last);

         return Element & To_Set;

      end To_Set;

   begin
      First := Fixed.Index
            (Source => Value,
             Set    => Spaces,
             Test   => Outside);
      return To_Set;
   end Split;

end AWS.Headers.Values;

