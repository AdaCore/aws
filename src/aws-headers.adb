------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2003                          --
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

with Ada.Strings.Unbounded;

with AWS.Net.Buffered;

package body AWS.Headers is

   --------------
   -- Get_Line --
   --------------

   function Get_Line
     (Headers : in List;
      N       : in Positive)
      return String
   is
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

   function Get_Values
     (Headers : in List;
      Name    : in String)
      return String
   is
      Values : constant VString_Array := Get_Values (Headers, Name);

      function Get_Values (Start_From : in Positive) return String;
      --  Return string of header values comma separated
      --  concateneted starting from Start_From index.

      ----------------
      -- Get_Values --
      ----------------

      function Get_Values (Start_From : in Positive) return String is
         Value : constant String
            := Ada.Strings.Unbounded.To_String (Values (Start_From));
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

   -----------------
   -- Send_Header --
   -----------------

   procedure Send_Header
     (Socket  : in Net.Socket_Type'Class;
      Headers : in List) is
   begin
      for J in 1 .. Count (Headers) loop
         Net.Buffered.Put_Line (Socket, Get_Line (Headers, J));
      end loop;
   end Send_Header;

end AWS.Headers;
