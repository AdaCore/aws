------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2000                            --
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

package body AWS.Parameters is

   function Image (N : Natural) return String;
   --  Returns N as a string without the leading space.

   ---------
   -- Add --
   ---------

   procedure Add
     (Parameter_Set : in out Set;
      Key, Value    : in     String)
   is
      C       : constant Positive := Parameter_Set.Count + 1;
      K_Key   : constant String   := "__AWS_K" & Image (C);
      K_Value : constant String   := "__AWS_V" & Image (C);
   begin
      Parameter_Set.Count := Parameter_Set.Count + 1;

      Parameters.Parameter_Set.Insert_Node
        ((To_Unbounded_String (Key), To_Unbounded_String (Value)),
         Parameter_Set.Data);

      Parameters.Parameter_Set.Insert_Node
        ((To_Unbounded_String (K_Key), To_Unbounded_String (Key)),
         Parameter_Set.Data);

      Parameters.Parameter_Set.Insert_Node
        ((To_Unbounded_String (K_Value), To_Unbounded_String (Value)),
         Parameter_Set.Data);
   end Add;

   -----------
   -- Count --
   -----------

   function Count (Parameter_Set : in Set) return Natural is
   begin
      return Parameters.Parameter_Set.Number_Of_Nodes (Parameter_Set.Data);
   end Count;

   -----------
   -- Exist --
   -----------

   function Exist (Parameter_Set : in Set; Key : in String) return Boolean is
      Item : KV;
   begin
      Parameters.Parameter_Set.Inquire (Key, Parameter_Set.Data, Item);
      return True;
   exception
      when others =>
         return False;
   end Exist;

   ---------
   -- Get --
   ---------

   function Get (Parameter_Set : in Set; Key : in String) return String is
      Item : KV;
   begin
      Parameters.Parameter_Set.Inquire (Key, Parameter_Set.Data, Item);
      return To_String (Item.Value);
   exception
      when others =>
         return "";
   end Get;

   -------------
   -- Get_Key --
   -------------

   function Get_Key
     (Parameter_Set : in Set;
      N             : in Positive)
     return String
   is
      Key  : constant String := "__AWS_K" & Image (N);
   begin
      return Get (Parameter_Set, Key);
   end Get_Key;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Parameter_Set : in Set;
      N             : in Positive)
      return String
   is
      Key  : constant String := "__AWS_V" & Image (N);
   begin
      return Get (Parameter_Set, Key);
   end Get_Value;

   -----------
   -- Image --
   -----------

   function Image (N : Natural) return String is
      NS : constant String := Natural'Image (N);
   begin
      return NS (2 .. NS'Last);
   end Image;

   -------------
   -- Key_For --
   -------------

   function Key_For (Item : in KV) return String is
   begin
      return To_String (Item.Key);
   end Key_For;

   -------------
   -- Release --
   -------------

   procedure Release (Parameter_Set : in out Set) is
   begin
      Parameters.Parameter_Set.Delete_Tree (Parameter_Set.Data);
   end Release;

end AWS.Parameters;

