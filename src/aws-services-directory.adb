------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2001                            --
--                      Dmitriy Anisimkov & Pascal Obry                     --
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

with Ada.Calendar;
with Ada.Characters.Handling;
with Ada.Strings.Unbounded;

with AWS.OS_Lib;
with AWS.Parameters;

with GNAT.Calendar.Time_IO;
with GNAT.Directory_Operations;

with Avl_Tree_Generic;

package body AWS.Services.Directory is

   ------------
   -- Browse --
   ------------

   function Browse
     (Directory_Name : in String;
      Request        : in AWS.Status.Data)
     return Translate_Table
   is

      use GNAT.Directory_Operations;
      use Ada.Strings.Unbounded;
      use Templates_Parser;

      type Order_By is (Orig,  -- original order, as read on the file system
                        Dir,   -- order by Directory flag
                        Name,  -- order by file/directory name
                        Time,  -- order by file time
                        Size); -- order by file time

      --  File Tree

      type File_Record is record
         Name      : Unbounded_String;
         Size      : Integer;
         Directory : Boolean;
         Time      : Ada.Calendar.Time;
         UID       : Natural;
      end record;

      function Key_For (File : in File_Record) return File_Record;
      pragma Inline (Key_For);

      function "<" (Left, Right : in File_Record) return Boolean;

      procedure Each_Entry
        (Item     : in out File_Record;
         Continue :    out Boolean);

      package File_Tree is new Avl_Tree_Generic
        (File_Record, File_Record, Key_For, "<");

      function End_Slash (Name : in String) return String;
      --  Return Name terminated with a directory separator.

      procedure For_Each_File is new
        File_Tree.In_Order_Tree_Traversal (Process => Each_Entry);

      Names  : Vector_Tag;
      Sizes  : Vector_Tag;
      Times  : Vector_Tag;
      Is_Dir : Vector_Tag;

      Direct_Ordr : Unbounded_String;
      Back_Ordr   : Unbounded_String;

      Ordr : array (Order_By'Range) of Unbounded_String;

      --  Composite with directory flag orders

      Dir_Ordr : array (Name .. Time) of Unbounded_String;

      Order_Set : array (1 .. Ordr'Length) of Order_By :=
         (1 => Dir, 2 => Name, others => Orig);

      Order_Asc : array (Order_Set'Range) of Boolean := (others => True);

      ---------------
      -- End_Slash --
      ---------------

      function End_Slash (Name : in String) return String is
      begin
         if Name /= ""
           and then Name (Name'Last) = '/'
         then
            return Name;
         else
            return Name & '/';
         end if;
      end End_Slash;

      -------------
      -- Key_For --
      -------------

      function Key_For (File : in File_Record) return File_Record is
      begin
         return File;
      end Key_For;

      ---------
      -- "<" --
      ---------

      function "<" (Left, Right : in File_Record) return Boolean is
         use type Ada.Calendar.Time;
      begin
         for I in Order_Set'Range loop

            case Order_Set (I) is

               when Dir =>
                  if Left.Directory /= Right.Directory then
                     return Left.Directory < Right.Directory xor Order_Asc (I);
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
                        return Left_Name < Right_Name xor not Order_Asc (I);
                     end if;
                  end;

               when Size =>
                  if Left.Size /= Right.Size then
                     return Left.Size < Right.Size xor not Order_Asc (I);
                  end if;

               when Time =>
                  if Left.Time /= Right.Time then
                     return Left.Time < Right.Time xor not Order_Asc (I);
                  end if;

               when Orig =>
                  return Left.UID < Right.UID xor not Order_Asc (I);
            end case;
         end loop;

         return Left.UID < Right.UID;
      end "<";

      Dir_Iterator : Dir_Type;
      URI          : constant String := End_Slash (AWS.Status.URI (Request));
      Dir_Entry    : String (1 .. 1_024);
      Param_List   : AWS.Parameters.List;
      Last         : Natural;

      Dir_Str      : constant String := End_Slash (Directory_Name);
      File_Entry   : File_Record;
      UID_Sq       : Natural := 0;

      use File_Tree;

      Order_Tree   : Avl_Tree;

      ----------------
      -- Each_Entry --
      ----------------

      procedure Each_Entry
        (Item     : in out File_Record;
         Continue :    out Boolean) is
      begin
         if Item.Directory then
            Sizes := Sizes & '-';
            Names := Names & End_Slash (To_String (Item.Name));
         else
            Sizes := Sizes & Integer'Image (Item.Size);
            Names := Names & Item.Name;
         end if;

         Times := Times &
           GNAT.Calendar.Time_IO.Image (Item.Time, "%Y/%m/%d %T");

         Is_Dir   := Is_Dir & Item.Directory;
         Continue := True;
      end Each_Entry;

      --  Table for quick False and True external representation look-up.

      Asc_Image : constant array (Boolean) of Character
        := (False => 'N', True => 'Y');

   begin
      Param_List := AWS.Status.Parameters (Request);

      for I in Order_Set'Range loop
         declare
            Ordr : constant String := AWS.Parameters.Get (Param_List, "O", I);
         begin
            if Ordr /= "" then
               Order_Set (I) := Order_By'Value (Ordr);
               Order_Asc (I) := AWS.Parameters.Get (Param_List, "A", I) = "Y";
            end if;

            Append (Direct_Ordr,
                    "&O=" & Order_By'Image (Order_Set (I)) &
                    "&A=" & Asc_Image (Order_Asc (I)));

            if I = Order_Set'First then
               Append (Back_Ordr,
                       "?O=" & Order_By'Image (Order_Set (I)) &
                       "&A=" & Asc_Image (not Order_Asc (I)));
            else
               Append (Back_Ordr,
                       "&O=" & Order_By'Image (Order_Set (I)) &
                       "&A=" & Asc_Image (Order_Asc (I)));
            end if;

            exit when Order_Set (I) = Orig;
         end;
      end loop;

      for I in Dir_Ordr'Range loop
         Dir_Ordr (I) :=
           To_Unbounded_String ("?A=Y&A=Y&O=Dir&O=" & Order_By'Image (I));
      end loop;

      if Order_Set (Order_Set'First) = Dir
        and then Order_Set (Order_Set'First + 1) in Dir_Ordr'Range
      then
         Dir_Ordr (Order_Set (Order_Set'First + 1)) :=
           To_Unbounded_String
           ("?O=Dir&A="
            & Asc_Image (not Order_Asc (Order_Set'First))
            & "&O=" & Order_By'Image (Order_Set (Order_Set'First + 1))
            & "&A=" & Asc_Image (not Order_Asc (Order_Set'First + 1)));
      end if;

      for I in Ordr'Range loop
         if I = Order_Set (Order_Set'First) then
            Ordr (I) := Back_Ordr;
         else
            Ordr (I) := "?A=Y&O=" & Order_By'Image (I) & Direct_Ordr;
         end if;
      end loop;

      --  Read directory entry.

      Open (Dir_Iterator, Dir_Str);

      loop
         Read (Dir_Iterator, Dir_Entry, Last);

         exit when Last = 0;

         declare
            Filename      : constant String := Dir_Entry (1 .. Last);
            Full_Pathname : constant String := Dir_Str & Filename;
         begin
            File_Entry.Directory := AWS.OS_Lib.Is_Directory (Full_Pathname);

            if File_Entry.Directory then
               File_Entry.Size := -1;
            else
               File_Entry.Size :=
                 Integer (AWS.OS_Lib.File_Size (Full_Pathname));
            end if;

            File_Entry.Name := To_Unbounded_String (Filename);
            File_Entry.Time := AWS.OS_Lib.File_Timestamp (Full_Pathname);
            File_Entry.UID  := UID_Sq;
            UID_Sq          := UID_Sq + 1;
         end;

         File_Tree.Insert_Node (File_Entry, Order_Tree);
      end loop;

      For_Each_File (Order_Tree);

      return (1 => Assoc ("URI", URI),
              2 => Assoc ("VERSION",   AWS.Version),
              3 => Assoc ("IS_DIR", Is_Dir),
              4 => Assoc ("NAMES", Names),
              5 => Assoc ("SIZES", Sizes),
              6 => Assoc ("TIMES", Times),
              7 => Assoc ("NAME_ORDR", Ordr (Name)),
              8 => Assoc ("DIR_ORDR", Ordr (Dir)),
              9 => Assoc ("SIZE_ORDR", Ordr (Size)),
             10 => Assoc ("TIME_ORDR", Ordr (Time)),
             11 => Assoc ("ORIG_ORDR", Ordr (Orig)),
             12 => Assoc ("DIR_NAME_ORDR", Dir_Ordr (Name)),
             13 => Assoc ("DIR_TIME_ORDR", Dir_Ordr (Time)));
   end Browse;

   ------------
   -- Browse --
   ------------

   function Browse
     (Directory_Name    : in String;
      Template_Filename : in String;
      Request           : in AWS.Status.Data;
      Translations      : in Translate_Table := No_Translation)
     return String is
   begin
      return Parse
        (Filename     => Template_Filename,
         Translations => Translations & Browse (Directory_Name, Request),
         Cached       => True);
   end Browse;

end AWS.Services.Directory;
