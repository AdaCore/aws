------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                          Copyright (C) 2002-2003                         --
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
      Mode        : Data_Mode;
   end record;

   package Res_Files is new Table_Of_Strings_And_Static_Values_G
     (Character, String, "<", "=", Node);

   Files_Table : Res_Files.Table_Type;

   Empty_Buffer : aliased constant Ada.Streams.Stream_Element_Array
     := (1 .. 0 => 0);

   procedure Read_Uncompressed
     (Resource : in out Z_File_Tagged;
      Buffer   :    out Stream_Element_Array;
      Last     :    out Stream_Element_Offset);
   --  Read data from Resource and returned them uncompressed

   -----------
   -- Close --
   -----------

   procedure Close (Resource : in out File_Tagged) is
      pragma Unreferenced (Resource);
   begin
      null;
   end Close;

   procedure Close (Resource : in out Z_File_Tagged) is
      use type Utils.Stream_Element_Array_Access;
   begin
      if Resource.U_Buffer /= null then
         ZLib.Close (Resource.U_Filter, Ignore_Error => True);
         Utils.Free (Resource.U_Buffer);
      end if;
   end Close;

   ------------
   -- Create --
   ------------

   procedure Create
     (File   :    out File_Type;
      Buffer : in     Buffer_Access;
      Mode   : in     Data_Mode     := Uncompressed) is
   begin
      if Mode = Uncompressed then
         File := new File_Tagged;
      else
         File := new Z_File_Tagged;
      end if;

      if Buffer = null then
         File_Tagged (File.all).Buffer := Empty_Buffer'Access;
      else
         File_Tagged (File.all).Buffer := Buffer;
      end if;

      File_Tagged (File.all).K := Buffer'First;
   end Create;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File (Resource : in File_Tagged) return Boolean is
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
      return Ada.Streams.Stream_Element_Offset is
   begin
      if Res_Files.Is_Present (Files_Table, Name) then
         return Res_Files.Value (Files_Table, Name).File_Buffer'Length;
      else
         raise Resource_Error;
      end if;
   end File_Size;

   --------------------
   -- File_Timestamp --
   --------------------

   function File_Timestamp (Name : in String) return Ada.Calendar.Time is
   begin
      if Res_Files.Is_Present (Files_Table, Name) then
         return Res_Files.Value (Files_Table, Name).File_Time;
      else
         raise Resource_Error;
      end if;
   end File_Timestamp;

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
     (File :    out File_Type;
      Name : in     String;
      Form : in     String    := "")
   is
      pragma Unreferenced (Form);
      N : Node;
   begin
      if Res_Files.Is_Present (Files_Table, Name) then
         N := Res_Files.Value (Files_Table, Name);

         if N.Mode = Uncompressed then
            File := new File_Tagged;
         else
            --  This is a compressed data
            File := new Z_File_Tagged;
         end if;

         File_Tagged (File.all).Buffer := N.File_Buffer;
         File_Tagged (File.all).K := N.File_Buffer'First;
      else
         File := null;
      end if;
   end Open;

   ----------
   -- Read --
   ----------

   procedure Read
     (Resource : in out File_Tagged;
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

   ----------
   -- Read --
   ----------

   procedure Read
     (Resource : in out Z_File_Tagged;
      Buffer   :    out Stream_Element_Array;
      Last     :    out Stream_Element_Offset)
   is
      use type Utils.Stream_Element_Array_Access;
   begin
      if Resource.U_Buffer /= null then
         --  Compressed resources are not supported and the resource is
         --  compressed.
         Read_Uncompressed (Resource, Buffer, Last);
      else
         Read (File_Tagged (Resource), Buffer, Last);
      end if;
   end Read;

   -----------------------
   -- Read_Uncompressed --
   -----------------------

   procedure Read_Uncompressed
     (Resource : in out Z_File_Tagged;
      Buffer   :    out Stream_Element_Array;
      Last     :    out Stream_Element_Offset)
   is

      procedure Read
        (Item : out Stream_Element_Array;
         Last : out Stream_Element_Offset);

      ----------
      -- Read --
      ----------

      procedure Read
        (Item : out Stream_Element_Array;
         Last : out Stream_Element_Offset) is
      begin
         Read (File_Tagged (Resource), Item, Last);
      end Read;

      procedure U_Read is new ZLib.Read
        (Read, Resource.U_Buffer.all, Resource.R_First, Resource.R_Last);

   begin
      U_Read (Resource.U_Filter, Buffer, Last);
   end Read_Uncompressed;

   --------------
   -- Register --
   --------------

   procedure Register
     (Name      : in String;
      Content   : in Buffer_Access;
      File_Time : in Calendar.Time;
      Mode      : in Data_Mode     := Uncompressed) is
   begin
      Res_Files.Insert (Files_Table, Name, (Content, File_Time, Mode));
   end Register;

   -----------
   -- Reset --
   -----------

   procedure Reset (Resource : in out File_Tagged) is
   begin
      Resource.K := Resource.Buffer'First;
   end Reset;

   ----------
   -- Size --
   ----------

   function Size (Resource : in File_Tagged) return Stream_Element_Offset is
   begin
      return Resource.Buffer'Length;
   end Size;

   ------------------------
   -- Support_Compressed --
   ------------------------

   procedure Support_Compressed
     (Resource : in out Z_File_Tagged;
      State    : in     Boolean) is
   begin
      if State = False then
         Resource.U_Buffer := new Stream_Element_Array (1 .. 4_096);
         ZLib.Inflate_Init (Resource.U_Filter);
         Resource.R_First := Resource.U_Buffer'Last + 1;
      end if;
   end Support_Compressed;

end AWS.Resources.Embedded;
