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

--  Parameters are put into an AVL Tree. Each entry in the tree is composed of
--  a Key and a Value. The parameters must be accessible through their name
--  and also using an index. So given a set of parameters (K1=V1, K2=V2...),
--  one must be able to ask for the value for K1 but also the name of the
--  second key or the value of the third key.
--
--  Each K/V pair is then insert into the tree three times:
--
--  1) key=K with value=V
--  2) key=__AWS_K<n> with value=K     (n beeing an indice representing the
--  3) key=__AWS_V<n> with value=V      entry number in the tree)
--
--  So to get the third key name we ask for the entry indexed under __AWS_K3.
--
--  Another important point is that a key can have many values. For example
--  with an HTML multiple select entry in a form. In such a case all values
--  associated with the key K are concatenated together with a specific
--  separator.

with Ada.Strings.Unbounded;

with AWS.Utils;

with Strings_Cutter;

package body AWS.Parameters is

   use Ada.Strings.Unbounded;

   Val_Separator : constant Character := ASCII.VT;

   function Internal_Get (Parameter_Set : in Set;
                          Key           : in String;
                          N             : in Natural)
                         return String;
   pragma Inline (Internal_Get);
   --  Returns the Nth value associated with Key into Parameter_Set. Returns
   --  the emptry string if key does not exist. If N = 0 it returns as-is all
   --  the values as inserted in the tree for Key.

   ---------
   -- Add --
   ---------

   procedure Add
     (Parameter_Set : in out Set;
      Key, Value    : in     String)
   is
      C       : constant Positive := Parameter_Set.Count + 1;
      K_Key   : constant String   := "__AWS_K" & Utils.Image (C);
      K_Value : constant String   := "__AWS_V" & Utils.Image (C);
   begin
      Parameter_Set.Count := Parameter_Set.Count + 1;

      begin
         Key_Value.Insert_Node
           ((To_Unbounded_String (Key), To_Unbounded_String (Value)),
            Parameter_Set.Data);
      exception
         --  This key already exist, catenate the new value to the old one
         --  separated with Val_Separator.

         when Key_Value.Tree.Duplicate_Key =>
            declare
               Current_Value : constant String :=
                 Internal_Get (Parameter_Set, Key, 0);
            begin
               Key_Value.Update_Node
                 ((To_Unbounded_String (Key),
                   To_Unbounded_String (Current_Value
                                        & Val_Separator
                                        & Value)),
                  Parameter_Set.Data);
            end;
      end;

      Key_Value.Insert_Node
        ((To_Unbounded_String (K_Key), To_Unbounded_String (Key)),
         Parameter_Set.Data);

      Key_Value.Insert_Node
        ((To_Unbounded_String (K_Value), To_Unbounded_String (Value)),
         Parameter_Set.Data);
   end Add;

   -----------
   -- Count --
   -----------

   function Count (Parameter_Set : in Set) return Natural is
   begin
      return Key_Value.Number_Of_Nodes (Parameter_Set.Data);
   end Count;

   function Count (Parameter_Set : in Set; Key : in String) return Natural is
      Item : Key_Value.Data;
      CS   : Strings_Cutter.Cutted_String;
   begin
      Key_Value.Inquire (Key, Parameter_Set.Data, Item);

      Strings_Cutter.Create (CS,
                             To_String (Item.Value),
                             String'(1 => Val_Separator));
      declare
         Result : constant Natural := Strings_Cutter.Field_Count (CS);
      begin
         Strings_Cutter.Destroy (CS);
         return Result;
      end;

   exception
      when others =>
         return 0;
   end Count;

   -----------
   -- Exist --
   -----------

   function Exist (Parameter_Set : in Set; Key : in String) return Boolean is
      Item : Key_Value.Data;
   begin
      Key_Value.Inquire (Key, Parameter_Set.Data, Item);
      return True;
   exception
      when others =>
         return False;
   end Exist;

   ---------
   -- Get --
   ---------

   function Get (Parameter_Set : in Set;
                 Key           : in String;
                 N             : in Positive := 1)
                return String is
   begin
      return Internal_Get (Parameter_Set, Key, N);
   end Get;

   -------------
   -- Get_Key --
   -------------

   function Get_Key
     (Parameter_Set : in Set;
      N             : in Positive)
     return String
   is
      Key  : constant String := "__AWS_K" & Utils.Image (N);
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
      Key  : constant String := "__AWS_V" & Utils.Image (N);
   begin
      return Get (Parameter_Set, Key);
   end Get_Value;

   ------------------
   -- Internal_Get --
   ------------------

   function Internal_Get (Parameter_Set : in Set;
                          Key           : in String;
                          N             : in Natural)
   return String is
      Item : Key_Value.Data;
      CS   : Strings_Cutter.Cutted_String;
   begin
      Key_Value.Inquire (Key, Parameter_Set.Data, Item);

      Strings_Cutter.Create (CS,
                             To_String (Item.Value),
                             String'(1 => Val_Separator));
      declare
         Result : constant String := Strings_Cutter.Field (CS, N);
      begin
         Strings_Cutter.Destroy (CS);
         return Result;
      end;

   exception
      when others =>
         return "";
   end Internal_Get;

   -------------
   -- Release --
   -------------

   procedure Release (Parameter_Set : in out Set) is
   begin
      Key_Value.Delete_Tree (Parameter_Set.Data);
   end Release;

end AWS.Parameters;
