------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                            Copyright (C) 2004                            --
--                               Pascal Obry                                --
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

package body Templates_Parser.Utils is

   -----------
   -- Image --
   -----------

   function Image (T : in Tag) return String is

      function Quote (Str : in String) return String;
      pragma Inline (Quote);
      --  Quote Str and double quote inside Str if needed

      -----------
      -- Quote --
      -----------

      function Quote (Str : in String) return String is
         Result : Unbounded_String;
      begin
         Append (Result, """");
         for K in Str'Range loop
            if Str (K) = '"' then
               Append (Result, """""");
            else
               Append (Result, Str (K));
            end if;
         end loop;
         Append (Result, """");
         return To_String (Result);
      end Quote;

      Result : Unbounded_String;
      N      : Tag_Node_Access := T.Data.Head;
   begin
      while N /= null loop
         if N.Kind = Value then
            if Result /= Null_Unbounded_String then
               Append (Result, ",");
            end if;
            Append (Result, Quote (To_String (N.V)));
         else
            Append (Result, Image (N.VS.all));
         end if;
         N := N.Next;
      end loop;

      return "(" & To_String (Result) & ")";
   end Image;

   -----------
   -- Value --
   -----------

   function Value (T : in String) return Tag is

      function Value (T : in String) return Tag;
      --  Returns the Tag for T string

      function Clear_Quote (Str : in String) return String;
      pragma Inline (Clear_Quote);
      --  Removes double quote in Str

      -----------------
      -- Clear_Quote --
      -----------------

      function Clear_Quote (Str : in String) return String is
         Result : Unbounded_String;
      begin
         for K in Str'Range loop
            if Str (K) /= '"'
              or else (K < Str'Last and then Str (K + 1) /= '"')
            then
               Append (Result, Str (K));
            end if;
         end loop;
         return To_String (Result);
      end Clear_Quote;

      -----------
      -- Value --
      -----------

      function Value (T : in String) return Tag is
         Result : Tag;
         K      : Natural := T'First;
         N      : Natural;
         Last   : Natural;
      begin
         while K < T'Last loop

            if T (K) = '(' then
               --  This is a nested Tag, Look for corresponding closing parent
               Last := K + 1;
               N    := 0;

               Nested_Tag : loop
                  if T (Last) = ')' then
                     if N = 0 then
                        --  Matching parent found, add it to the result
                        Result := Result & Value (T (K + 1 .. Last));
                        K := Last;
                        --  and leave this loop
                        exit Nested_Tag;
                     else
                        N := N - 1;
                     end if;

                  elsif T (Last) = '(' then
                     N := N + 1;
                  end if;

                  if Last = T'Last then
                     --  Matching parent not found
                     raise Constraint_Error;
                  else
                     Last := Last + 1;
                  end if;
               end loop Nested_Tag;

            elsif T (K) = '"' then
               --  This is a value, Look for corresponding closing quote
               Last := K + 1;

               Quoted_Value : loop
                  if T (Last) = '"'
                    and then Last < T'Last
                    and then T (Last + 1) = '"'
                  then
                     --  Skip this quote
                     Last := Last + 1;

                  elsif T (Last) = '"'
                    and then (Last = T'Last or else T (Last + 1) /= '"')
                  then
                     --  Found matching quote, add this value
                     Result := Result & Clear_Quote (T (K + 1 .. Last - 1));
                     K := Last;
                     --  and leave loop
                     exit Quoted_Value;

                  elsif Last = T'Last then
                     --  No matching quote
                     raise Constraint_Error;
                  end if;
                  Last := Last + 1;
               end loop Quoted_Value;

               --  Here we must have either a ',' or ")"
               if Last /= T'Last
                 and then T (Last + 1) /= ','
                 and then T (Last + 1) /= ')'
               then
                  raise Constraint_Error;
               end if;
            end if;

            K := K + 1;
         end loop;

         return Result;
      end Value;

   begin
      if T'Length > 1 and then T (T'First) = '(' and then T (T'Last) = ')' then
         return Value (T (T'First + 1 .. T'Last - 1));
      else
         raise Constraint_Error;
      end if;
   end Value;

end Templates_Parser.Utils;
