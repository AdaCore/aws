------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2002                            --
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

with Table_Of_Strings_And_Static_Values_G;

package body AWS.Resources.Embedded is

   type Node is record
      File_Buffer : Buffer_Access;
      File_Time   : Calendar.Time;
   end record;

   package Res_Files is new Table_Of_Strings_And_Static_Values_G
     (Character, String, "<", "=", Node);

   Files_Table : Res_Files.Table_Type;

   -----------
   -- Close --
   -----------

   procedure Close (Resource : in out File_Type) is
      pragma Unreferenced (Resource);
   begin
      null;
   end Close;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File (Resource : in File_Type) return Boolean is
   begin
      return Resource.K > Resource.Buffer'Last;
   end End_Of_File;

   ------------
   -- Exists --
   ------------

   function Exists (Name : in String) return Boolean is
   begin
      return Res_Files.Is_Present (Files_Table, Name);
   end Exists;

   ---------------
   -- File_Size --
   ---------------

   function File_Size
     (Name : in String)
      return Ada.Streams.Stream_Element_Offset
   is
      N : Node;
   begin
      if Res_Files.Is_Present (Files_Table, Name) then
         N := Res_Files.Value (Files_Table, Name);
         return N.File_Buffer'Length;
      else
         raise Resource_Error;
      end if;
   end File_Size;

   --------------------
   -- File_Timestamp --
   --------------------

   function File_Timestamp (Name : in String) return Ada.Calendar.Time is
      N : Node;
   begin
      if Res_Files.Is_Present (Files_Table, Name) then
         N := Res_Files.Value (Files_Table, Name);
         return N.File_Time;
      else
         raise Resource_Error;
      end if;
   end File_Timestamp;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line
     (Resource  : in out File_Type;
      Buffer    :    out String;
      Last      :    out Natural)
   is
      CR : Stream_Element := 13;
      LF : Stream_Element := 10;

      K : Stream_Element_Offset renames Resource.K;
   begin
      if K = Resource.Buffer'Last then
         Last := 0;
      else
         Last := Buffer'First - 1;

         for I in K .. Resource.Buffer'Last loop

            if Resource.Buffer (I) = LF then     -- UNIX style line terminator
               Resource.K := I + 1;
               exit;

            elsif Resource.Buffer (I) = CR       -- DOS style line terminator
              and then I + 1 <= Resource.Buffer'Last
              and then Resource.Buffer (I + 1) = LF
            then
               Resource.K := I + 2;
               exit;

            else
               Last := Last + 1;
               Buffer (Last) := Character'Val (Resource.Buffer (I));
            end if;
         end loop;
      end if;
   end Get_Line;

   ---------------------
   -- Is_Regular_File --
   ---------------------

   function Is_Regular_File (Name : in String) return Boolean is
   begin
      return Exists (Name);
   end Is_Regular_File;

   ----------
   -- Open --
   ----------

   procedure Open
     (File :    out File_Access;
      Name : in     String;
      Form : in     String    := "")
   is
      pragma Unreferenced (Form);
      N : Node;
   begin
      if Res_Files.Is_Present (Files_Table, Name) then
         N := Res_Files.Value (Files_Table, Name);

         File := new File_Type'
           (Resources.File_Type with N.File_Buffer, N.File_Buffer'First);
      else
         File := null;
      end if;
   end Open;

   ----------
   -- Read --
   ----------

   procedure Read
     (Resource : in out File_Type;
      Buffer   :    out Stream_Element_Array;
      Last     :    out Stream_Element_Offset)
   is
      K    : Stream_Element_Offset renames Resource.K;
      Size : Stream_Element_Offset; --  Number of byte remaining in buffer
   begin
      if K > Resource.Buffer'Last then
         Last := 0;
      else
         Size := Resource.Buffer'Length - (K - Resource.Buffer'First);

         if Buffer'Length <= Size then
            Buffer := Resource.Buffer (K .. K + Buffer'Length - 1);
            Last := Buffer'Last;
            K := K + Buffer'Length;
         else
            Last := Buffer'First + Size - 1;
            Buffer (Buffer'First .. Last)
              := Resource.Buffer (K .. Resource.Buffer'Last);
            K := Resource.Buffer'Last + 1;
         end if;
      end if;

      Resource.K := K;
   end Read;

   --------------
   -- Register --
   --------------

   procedure Register
     (Name      : in String;
      Content   : in Buffer_Access;
      File_Time : in Calendar.Time) is
   begin
      Res_Files.Insert (Files_Table, Name, (Content, File_Time));
   end Register;

end AWS.Resources.Embedded;
