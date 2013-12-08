------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2013, AdaCore                     --
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

pragma Ada_2012;

with Ada.Calendar;
with Ada.Characters.Handling;
with Ada.Containers.Ordered_Sets;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;

with AWS.MIME;
with AWS.Parameters;
with AWS.Utils;

with GNAT.Calendar.Time_IO;

package body AWS.Services.Directory is

   use Ada;
   use Ada.Strings.Unbounded;

   type File_Record is record
      Name      : Unbounded_String;
      Size      : Utils.File_Size_Type;
      Directory : Boolean;
      Time      : Calendar.Time;
      UID       : Natural;
      Order_Set : Unbounded_String;
   end record;

   function "<" (Left, Right : File_Record) return Boolean;

   overriding function "=" (Left, Right : File_Record) return Boolean
     with Inline;

   package File_Tree is
     new Ada.Containers.Ordered_Sets (File_Record, "<", "=");

   type Order_Mode is
     (O,  -- original order, as read on the file system
      D,  -- order by Directory flag
      M,  -- order by MIME content type
      E,  -- order by file extention case insensitive
      X,  -- order by file extention case sensitive
      N,  -- order by file/directory name case insensitive
      A,  -- order by file/directory name case sensitive
      T,  -- order by file time
      S); -- order by file size

   Dir   : constant Order_Mode := D;
   MIME  : constant Order_Mode := M;
   Ext   : constant Order_Mode := E;
   SExt  : constant Order_Mode := X;
   Name  : constant Order_Mode := N;
   SName : constant Order_Mode := A;
   Size  : constant Order_Mode := S;
   Time  : constant Order_Mode := T;
   Orig  : constant Order_Mode := O;

   subtype Order_Char is Character;

   function To_Order_Mode (C : Order_Char) return Order_Mode;
   --  Returns the Order_Mode value for the Order_Char. See Order_Set
   --  comments for the data equivalence table.

   function To_Order_Char (O : Order_Mode) return Order_Char;
   --  Returns the Order_Char for the Order_Mode. This routine is the
   --  above reverse function.

   function Get_Ext (File_Name : String) return String;
   --  Returns file extension for File_Name and the empty string if there
   --  is not extension.

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : File_Record) return Boolean is
      use type Ada.Calendar.Time;
      use type Utils.File_Size_Type;
      use AWS.MIME;

      Order_Item : Order_Mode;
      Ascending  : Boolean;
      O_C        : Order_Char;

   begin
      for I in 1 .. Length (Left.Order_Set) loop

         O_C        := Element (Left.Order_Set, I);
         Order_Item := To_Order_Mode (O_C);
         Ascending  := Characters.Handling.Is_Upper (O_C);

         case Order_Item is

            when Dir =>

               if Left.Directory /= Right.Directory then
                  return Left.Directory < Right.Directory xor Ascending;
               end if;

            when MIME =>

               if Left.Directory /= Right.Directory then
                  return Left.Directory < Right.Directory xor Ascending;

               elsif not Left.Directory and then not Right.Directory then
                  declare
                     Mime_Left  : constant String
                       := Content_Type (To_String (Left.Name));
                     Mime_Right : constant String
                       := Content_Type (To_String (Right.Name));
                  begin
                     if Mime_Left /= Mime_Right then
                        return Mime_Left < Mime_Right xor not Ascending;
                     end if;
                  end;
               end if;

            when Ext  =>

               if Left.Directory /= Right.Directory then
                  return Left.Directory < Right.Directory xor Ascending;

               elsif not Left.Directory and then not Right.Directory then
                  declare
                     use Ada.Characters.Handling;
                     Ext_Left  : constant String
                       := To_Upper (Get_Ext (To_String (Left.Name)));
                     Ext_Right : constant String
                       := To_Upper (Get_Ext (To_String (Right.Name)));
                  begin
                     if Ext_Left /= Ext_Right then
                        return Ext_Left < Ext_Right xor not Ascending;
                     end if;
                  end;
               end if;

            when SExt  =>

               if Left.Directory /= Right.Directory then
                  return Left.Directory < Right.Directory xor Ascending;

               elsif not Left.Directory and then not Right.Directory then
                  declare
                     Ext_Left  : constant String
                       := Get_Ext (To_String (Left.Name));
                     Ext_Right : constant String
                       := Get_Ext (To_String (Right.Name));
                  begin
                     if Ext_Left /= Ext_Right then
                        return Ext_Left < Ext_Right xor not Ascending;
                     end if;
                  end;
               end if;

            when Name =>

               declare
                  use Ada.Characters.Handling;
                  Left_Name  : constant  String
                    := To_Upper (To_String (Left.Name));
                  Right_Name : constant String :=
                                 To_Upper (To_String (Right.Name));
               begin
                  if Left_Name /= Right_Name then
                     return Left_Name < Right_Name xor not Ascending;
                  end if;
               end;

            when SName =>

               declare
                  Left_Name  : constant String := To_String (Left.Name);
                  Right_Name : constant String := To_String (Right.Name);
               begin
                  if Left_Name /= Right_Name then
                     return Left_Name < Right_Name xor not Ascending;
                  end if;
               end;

            when Size =>

               if Left.Size /= Right.Size then
                  return Left.Size < Right.Size xor not Ascending;
               end if;

            when Time =>

               if Left.Time /= Right.Time then
                  return Left.Time < Right.Time xor not Ascending;
               end if;

            when Orig =>

               return Left.UID < Right.UID xor not Ascending;

         end case;
      end loop;

      return Left.UID < Right.UID;
   end "<";

   ---------
   -- "=" --
   ---------

   overriding function "=" (Left, Right : File_Record) return Boolean is
   begin
      --  Can't be equal as all File_Record ID are uniq
      pragma Assert (Left.UID /= Right.UID);
      return False;
   end "=";

   ------------
   -- Browse --
   ------------

   function Browse
     (Directory_Name : String;
      Request        : AWS.Status.Data) return Translate_Set
   is
      Set : Translate_Set;

      Max_Order_Length : constant := 8;

      Default_Order : constant String := "DN";

      --  File Tree

      function Invert (C : Order_Char) return Order_Char;
      --  Return the reverse order for C. It means that the upper case letter
      --  is change to a lower case and a lower case letter to an upper case
      --  one.

      procedure Each_Entry (Cursor : File_Tree.Cursor);
      --  Iterator callback procedure

      procedure Read_Directory (Directory_Name : String);
      --  Read Dir_Name entries and insert them into the Order_Tree table

      Names  : Vector_Tag;
      Sizes  : Vector_Tag;
      Times  : Vector_Tag;
      Is_Dir : Vector_Tag;

      Direct_Ordr : Unbounded_String;
      --  Direct ordering rules

      Back_Ordr   : Unbounded_String;
      --  Reverse ordering rules. This rules is the opposite of the above one

      Order_Set   : Unbounded_String;
      --  This variable is set with the order rules from the Web page ORDER
      --  variable. The value is a set of characters each one represent an
      --  ordering key:
      --    'D'   directory order
      --    'M'   MIME type order
      --    'E'   extension order
      --    'X'   extension (case sensitive) order
      --    'N'   name order
      --    'A'   name (case sensitive) order
      --    'S'   size order
      --    'T'   time order
      --
      --  Furthermore, an upper-case character means an ascending order and a
      --  lower-case character means a descending order.

      Ordr         : array (Order_Mode'Range) of Unbounded_String;
      --  This table will receive the string rule (a set of Order_Char) for
      --  each order.

      Param_List   : constant AWS.Parameters.List :=
                       AWS.Status.Parameters (Request);
      --  Web parameter's list

      Mode         : constant String :=
                       AWS.Parameters.Get (Param_List, "MODE");

      Mode_Param   : constant String := "?MODE=" & Mode;
      --  MODE Web variable is set either to True or False to toggle the
      --  simple ordering rules or the complex one.

      subtype Dir_Order_Range is Order_Mode range Name .. Time;

      Dir_Ordr     : array (Dir_Order_Range) of Unbounded_String :=
        (Name  => To_Unbounded_String (Mode_Param & "&ORDER=DN"),
         SName => To_Unbounded_String (Mode_Param & "&ORDER=DA"),
         Time  => To_Unbounded_String (Mode_Param & "&ORDER=DT"));
      --  Defaults rules to order the directories by Name or by Time

      UID_Sq       : Natural := 0;

      use File_Tree;

      Order_Tree   : File_Tree.Set;

      ----------------
      -- Each_Entry --
      ----------------

      procedure Each_Entry (Cursor : File_Tree.Cursor) is
         Item : constant File_Record := File_Tree.Element (Cursor);
      begin
         if Item.Directory then
            Sizes := Sizes & '-';
            Names := Names & (Item.Name & '/');

         else
            Sizes := Sizes & Utils.File_Size_Type'Image (Item.Size);
            Names := Names & Item.Name;
         end if;

         Times  := Times
           & GNAT.Calendar.Time_IO.Image (Item.Time, "%Y/%m/%d %T");

         Is_Dir := Is_Dir & Item.Directory;
      end Each_Entry;

      ------------
      -- Invert --
      ------------

      function Invert (C : Character) return Character is
      begin
         if Characters.Handling.Is_Upper (C) then
            return Characters.Handling.To_Lower (C);
         else
            return Characters.Handling.To_Upper (C);
         end if;
      end Invert;

      Dir_Str : constant String := Utils.Normalized_Directory (Directory_Name);

      --------------------
      -- Read_Directory --
      --------------------

      procedure Read_Directory (Directory_Name : String) is

         procedure Insert
           (Filename     : String;
            Is_Directory : Boolean;
            Quit         : in out Boolean);

         ------------------------------
         -- Insert_Directory_Entries --
         ------------------------------

         procedure Insert_Directory_Entries is
            new Utils.For_Every_Directory_Entry (Insert);

         ------------
         -- Insert --
         ------------

         procedure Insert
           (Filename     : String;
            Is_Directory : Boolean;
            Quit         : in out Boolean)
         is
            Full_Pathname : constant String := Dir_Str & Filename;
            File_Entry    : File_Record;
            Cursor        : File_Tree.Cursor;
            Success       : Boolean;
         begin
            File_Entry.Directory := Is_Directory;

            if Is_Directory then
               File_Entry.Size := 0;
            else
               File_Entry.Size := Utils.File_Size (Full_Pathname);
            end if;

            File_Entry.Name      := To_Unbounded_String (Filename);
            File_Entry.Time      := Utils.File_Time_Stamp (Full_Pathname);
            File_Entry.UID       := UID_Sq;
            File_Entry.Order_Set := Order_Set;

            UID_Sq := UID_Sq + 1;

            File_Tree.Insert (Order_Tree, File_Entry, Cursor, Success);

            Quit := False;
         end Insert;

      begin
         Insert_Directory_Entries (Directory_Name);
      end Read_Directory;

   begin
      --  Read ordering rules from the Web page and build the direct and
      --  reverve rules.

      declare

         function Get_Order return String;
         --  Get current ordering string, if no ordering retrieve we set a
         --  default ordering.

         ---------------
         -- Get_Order --
         ---------------

         function Get_Order return String is
            P_Order : constant String :=
                        AWS.Parameters.Get (Param_List, "ORDER");
         begin
            if P_Order = "" then
               --  no ordering define, use the default one
               return Default_Order;

            elsif P_Order'Length > Max_Order_Length then
               return Strings.Fixed.Head (P_Order, Max_Order_Length);

            else
               return P_Order;
            end if;
         end Get_Order;

         Order : constant String := Get_Order;

      begin
         Order_Set := To_Unbounded_String (Order);

         for K in Order'Range loop
            Append (Direct_Ordr, Order (K));

            if K = Order'First then
               --  ???
               Append (Back_Ordr, Invert (Order (K)));
            else
               Append (Back_Ordr, Order (K));
            end if;
         end loop;
      end;

      --  Check if the directory ordering rules needs to be reverted

      if Length (Order_Set) >= 2
        and then To_Order_Mode (Element (Order_Set, 1)) = Dir
        and then To_Order_Mode (Element (Order_Set, 2)) in Dir_Order_Range
      then
         --  The current rule is a directory ordering, just invert it
         Dir_Ordr (To_Order_Mode (Element (Order_Set, 2))) :=
           To_Unbounded_String
             (Mode_Param
              & "&ORDER="
              & Invert (Element (Order_Set, 1))
              & Invert (Element (Order_Set, 2)));
      end if;

      --  Build the Ordr table for each kind of ordering

      for K in Ordr'Range loop
         if Length (Order_Set) >= 1
           and then K = To_Order_Mode (Element (Order_Set, 1))
         then
            --  This is the current rule, reverse it
            Ordr (K) := Mode_Param & "&ORDER=" & Back_Ordr;

         else
            Ordr (K) := Mode_Param
              & "&ORDER=" & To_Order_Char (K) & Direct_Ordr;
         end if;
      end loop;

      --  Read directory entries and insert each one on the Order_Tree. This
      --  will be inserted with the right order, as defined by the rules
      --  above.

      Read_Directory (Directory_Name);

      --  Iterate through the tree and fill the vector tag before insertion
      --  into the translate table.

      Iterate (Order_Tree, Each_Entry'Access);

      Clear (Order_Tree);

      Insert (Set, Assoc ("URI",           Utils.Normalized_Directory
                                             (AWS.Status.URI (Request))));
      Insert (Set, Assoc ("VERSION",       AWS.Version));
      Insert (Set, Assoc ("IS_DIR_V",      Is_Dir));
      Insert (Set, Assoc ("NAME_V",        Names));
      Insert (Set, Assoc ("SIZE_V",        Sizes));
      Insert (Set, Assoc ("TIME_V",        Times));
      Insert (Set, Assoc ("DIR_ORDR",      Ordr (Dir)));
      Insert (Set, Assoc ("MIME_ORDR",     Ordr (MIME)));
      Insert (Set, Assoc ("EXT_ORDR",      Ordr (Ext)));
      Insert (Set, Assoc ("SEXT_ORDR",     Ordr (SExt)));
      Insert (Set, Assoc ("NAME_ORDR",     Ordr (Name)));
      Insert (Set, Assoc ("SNME_ORDR",     Ordr (SName)));
      Insert (Set, Assoc ("SIZE_ORDR",     Ordr (Size)));
      Insert (Set, Assoc ("TIME_ORDR",     Ordr (Time)));
      Insert (Set, Assoc ("ORIG_ORDR",     Ordr (Orig)));
      Insert (Set, Assoc ("MODE",          Mode));
      Insert (Set, Assoc ("DIR_NAME_ORDR", Dir_Ordr (Name)));
      Insert (Set, Assoc ("DIR_SNME_ORDR", Dir_Ordr (SName)));
      Insert (Set, Assoc ("DIR_TIME_ORDR", Dir_Ordr (Time)));

      return Set;
   end Browse;

   ------------
   -- Browse --
   ------------

   function Browse
     (Directory_Name    : String;
      Template_Filename : String;
      Request           : AWS.Status.Data;
      Translations      : Translate_Set := Null_Set) return String
   is
      Set : Translate_Set := Browse (Directory_Name, Request);
   begin
      Insert (Set, Translations);

      return Parse
        (Filename => Template_Filename, Translations => Set, Cached => True);
   end Browse;

   -------------
   -- Get_Ext --
   -------------

   function Get_Ext (File_Name : String) return String is
      use Ada.Strings;

      Pos : constant Natural :=
              Fixed.Index (File_Name, Maps.To_Set ("."), Going => Backward);

   begin
      if Pos = 0 then
         return "";
      else
         return File_Name (Pos .. File_Name'Last);
      end if;
   end Get_Ext;

   -------------------
   -- To_Order_Char --
   -------------------

   function To_Order_Char (O : Order_Mode) return Order_Char is
   begin
      return Order_Mode'Image (O) (1);
   end To_Order_Char;

   -------------------
   -- To_Order_Mode --
   -------------------

   function To_Order_Mode (C : Order_Char) return Order_Mode is
   begin
      return Order_Mode'Value (String'(1 => C));
   end To_Order_Mode;

end AWS.Services.Directory;
