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

with Ada.Strings.Unbounded;

with AWS.Net.Buffered;

package body AWS.Headers is

   use Ada;
   use Ada.Strings.Unbounded;

   --------------
   -- Get_Line --
   --------------

   function Get_Line (Headers : List; N : Positive) return String is
      Pair : constant Element := Get (Headers, N);
   begin
      if Pair.Name = "" then
         return "";
      else
         return Pair.Name & ": " & Pair.Value;
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
         L := L
           + AWS.Headers.Get_Line
               (Headers => Headers,
                N       => J)'Length + 2;
      end loop;

      return L;
   end Length;

   -----------------
   -- Send_Header --
   -----------------

   procedure Send_Header
     (Socket : Net.Socket_Type'Class; Headers : List) is
   begin
      for J in 1 .. Count (Headers) loop
         Net.Buffered.Put_Line (Socket, Get_Line (Headers, J));
      end loop;
   end Send_Header;

end AWS.Headers;
