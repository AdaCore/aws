------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                     Dmitriy Anisimkov & Pascal Obry                      --
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
--  Each K/V pair is then inserted into the tree three times:
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

   -----------
   -- Count --
   -----------

   function Count (Parameter_List : in List) return Natural is
   begin
      return Key_Value.Number_Of_Nodes (Parameter_List.Data);
   end Count;

   -----------
   -- Count --
   -----------

   function Count
     (Parameter_List : in List;
      Name           : in String)
     return Natural
   is
      Item : Key_Value.Data;
      CS   : Strings_Cutter.Cutted_String;
   begin
      Key_Value.Inquire (Name, Parameter_List.Data, Item);

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

   function Exist
     (Parameter_List : in List;
      Name           : in String)
     return Boolean
   is
      Item : Key_Value.Data;
   begin
      Key_Value.Inquire (Name, Parameter_List.Data, Item);
      return True;
   exception
      when others =>
         return False;
   end Exist;

   ---------
   -- Get --
   ---------

   function Get
     (Parameter_List : in List;
      Name           : in String;
      N              : in Positive := 1)
     return String is
   begin
      return Internal_Get (Parameter_List, Name, N);
   end Get;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Parameter_List : in List;
      N              : in Positive := 1)
     return String
   is
      Key  : constant String := "__AWS_K" & Utils.Image (N);
   begin
      return Get (Parameter_List, Key);
   end Get_Name;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Parameter_List : in List;
      N              : in Positive := 1)
     return String
   is
      Key  : constant String := "__AWS_V" & Utils.Image (N);
   begin
      return Get (Parameter_List, Key);
   end Get_Value;

   ------------------
   -- Internal_Get --
   ------------------

   function Internal_Get
     (Parameter_List : in List;
      Name           : in String;
      N              : in Natural)
     return String
   is
      Item : Key_Value.Data;
      CS   : Strings_Cutter.Cutted_String;
   begin
      Key_Value.Inquire (Name, Parameter_List.Data, Item);

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

end AWS.Parameters;
