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

--  Parameters are put into an AVL Tree. Each entry in the tree is composed of
--  a Key and a Value. The parameters must be accessible through their name
--  and also using an index. So given a set of parameters (K1=V1, K2=V2...),
--  one must be able to ask for the value for K1 but also the name of the
--  second key or the value of the third key.
--
--  Each K/V pair is then inserted into the Data tree and two times into the
--  HTTP_Data tree:
--
--  Into Data:
--
--  1) key=K with value=V
--
--  Into HTTP_Data:
--
--  1) key=__AWS_K<n> with value=K     (n beeing an indice representing the
--  2) key=__AWS_V<n> with value=V      entry number in the tree)
--
--  So to get the third key name we ask for the entry indexed under __AWS_K3
--  into HTTP_Data tree.
--
--  Another important point is that a key can have many values. For example
--  with an HTML multiple select entry in a form. In such a case all values
--  associated with the key K are concatenated together with a specific
--  separator.

with AWS.Utils;

with Strings_Cutter;

package body AWS.Parameters is

   use Ada.Strings.Unbounded;

   -----------
   -- Count --
   -----------

   function Count (Parameter_List : in List) return Natural is
      use type Key_Value.Set_Access;
   begin
      if Parameter_List.HTTP_Data = null then
         return 0;
      else
         return Key_Value.Size (Parameter_List.HTTP_Data.all) / 2;
      end if;
   end Count;

   -----------
   -- Count --
   -----------

   function Count
     (Parameter_List : in List;
      Name           : in String)
     return Natural
   is
      Value : Unbounded_String;
      CS    : Strings_Cutter.Cutted_String;
   begin
      Key_Value.Get_Value (Parameter_List.Data.all, Name, Value);

      Strings_Cutter.Create
        (CS,
         To_String (Value),
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
     return Boolean is
   begin
      return Key_Value.Is_Present (Parameter_List.Data.all, Name);
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
      return To_String (Key_Value.Value (Parameter_List.HTTP_Data.all, Key));
   exception
      when others =>
         return "";
   end Get_Name;

   ---------------
   -- Get_Names --
   ---------------

   function Get_Names (Parameter_List : in List) return VString_Array is
      use type Key_Value.Set_Access;

      procedure Process
        (Key      : in     String;
         Value    : in     Unbounded_String;
         Order    : in     Positive;
         Continue : in out Boolean);

      Result : VString_Array (1 .. Name_Count (Parameter_List));

      -------------
      -- Process --
      -------------

      procedure Process
        (Key      : in     String;
         Value    : in     Unbounded_String;
         Order    : in     Positive;
         Continue : in out Boolean) is
      begin
         Result (Order) := To_Unbounded_String (Key);
      end Process;

      --------------------
      -- Each_Key_Value --
      --------------------

      procedure Each_Key_Value is
         new Key_Value.Table.Disorder_Traverse_G (Process);

   begin
      if Parameter_List.Data /= null then
         Each_Key_Value (Key_Value.Table.Table_Type (Parameter_List.Data.all));
      end if;

      return Result;
   end Get_Names;

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
      return To_String (Key_Value.Value (Parameter_List.HTTP_Data.all, Key));
   exception
      when others =>
         return "";
   end Get_Value;

   ----------------
   -- Get_Values --
   ----------------

   function Get_Values (Parameter_List : in List; Name : String)
      return VString_Array
   is
      Value : Unbounded_String;
      CS    : Strings_Cutter.Cutted_String;
   begin
      Key_Value.Get_Value (Parameter_List.Data.all, Name, Value);

      Strings_Cutter.Create
        (CS,
         To_String (Value),
         String'(1 => Val_Separator));

      declare
         Result : VString_Array (1 .. Strings_Cutter.Field_Count (CS));
      begin
         for I in Result'Range loop
            Result (I) := To_Unbounded_String (Strings_Cutter.Field (CS, I));
         end loop;
         Strings_Cutter.Destroy (CS);
         return Result;
      end;

   exception
      when others =>
         return (1 .. 0 => Null_Unbounded_String);
   end Get_Values;

   ------------------
   -- Internal_Get --
   ------------------

   function Internal_Get
     (Parameter_List : in List;
      Name           : in String;
      N              : in Natural)
     return String
   is
      Value : Unbounded_String;
      CS    : Strings_Cutter.Cutted_String;
   begin
      Key_Value.Get_Value (Parameter_List.Data.all, Name, Value);

      Strings_Cutter.Create (CS,
                             To_String (Value),
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

   ----------------
   -- Name_Count --
   ----------------

   function Name_Count (Parameter_List : in List) return Natural is
      use type Key_Value.Set_Access;
   begin
      if Parameter_List.Data = null then
         return 0;
      else
         return Key_Value.Size (Parameter_List.Data.all);
      end if;
   end Name_Count;

   ----------------
   -- URI_Format --
   ----------------

   function URI_Format (Parameter_List : in List) return String is
   begin
      return To_String (Parameter_List.Parameters);
   end URI_Format;

end AWS.Parameters;
