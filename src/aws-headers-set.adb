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

   -----------
   -- Parse --
   -----------

   procedure Parse (Headers : in out List; Line : in String) is
      use Ada.Strings;
      Delimiter_Index : constant Natural := Fixed.Index (Line, ":");
   begin
      if Debug_Flag then
         Ada.Text_IO.Put_Line ('>' & Line);
      end if;

      if Delimiter_Index = 0 then
         --  No delimiter, this is not a valid Header Line
         raise Format_Error;
      end if;

      Add (Headers,
           Line (Line'First .. Delimiter_Index - 1),
           Fixed.Trim (Line (Delimiter_Index + 1 .. Line'Last), Both));
   end Parse;

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
