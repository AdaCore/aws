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
with AWS.Resources.Streams.ZLib;

package body AWS.Resources.Streams.Embedded is

   type Node is record
      File_Buffer : Buffer_Access;
      File_Time   : Calendar.Time;
   end record;

   package Res_Files is new Table_Of_Strings_And_Static_Values_G
     (Character, String, "<", "=", Node);

   Files_Table : Res_Files.Table_Type;

   ------------
   -- Create --
   ------------

   procedure Create
     (File   :    out File_Type;
      Buffer : in     Buffer_Access)
   is
      Stream : Streams.Stream_Access;
   begin
      Stream := new Streams.Memory.Stream_Type;

      Streams.Memory.Append
        (Streams.Memory.Stream_Type (Stream.all), Buffer);

      Streams.Create (File, Stream);
   end Create;

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
      N     : Node;
      Found : Boolean;
   begin
      Res_Files.Get_Value (Files_Table, Name, N, Found);

      if Found then
         return N.File_Buffer'Length;
      end if;

      Res_Files.Get_Value (Files_Table, Name & ".gz", N, Found);

      if Found then
         return N.File_Buffer'Length;
      else
         raise Resource_Error;
      end if;
   end File_Size;

   --------------------
   -- File_Timestamp --
   --------------------

   function File_Timestamp (Name : in String) return Ada.Calendar.Time is
      N     : Node;
      Found : Boolean;
   begin
      Res_Files.Get_Value (Files_Table, Name, N, Found);

      if Found then
         return N.File_Time;
      end if;

      Res_Files.Get_Value (Files_Table, Name & ".gz", N, Found);

      if Found then
         return N.File_Time;
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
      Form : in     String    := "";
      GZip : in out Boolean)
   is
      pragma Unreferenced (Form);
      N        : Node;
      Found    : Boolean;

      procedure Open_File (Decompress : Boolean);

      ---------------
      -- Open_File --
      ---------------

      procedure Open_File (Decompress : Boolean) is
         Stream : Streams.Stream_Access := new Streams.Memory.Stream_Type;
      begin
         Streams.Memory.Append
           (Streams.Memory.Stream_Type (Stream.all), N.File_Buffer);

         if Decompress then
            Stream := Streams.ZLib.Inflate_Create (Stream);
         end if;

         Streams.Create (File, Stream);
      end Open_File;

   begin
      if GZip then
         Res_Files.Get_Value (Files_Table, Name & ".gz", N, Found);

         if Found then
            Open_File (False);
         end if;

         Res_Files.Get_Value (Files_Table, Name, N, Found);

         if Found then
            Open_File (False);
            GZip := False;
         else
            File := null;
         end if;
      else
         Res_Files.Get_Value (Files_Table, Name, N, Found);

         if Found then
            Open_File (False);
         end if;

         Res_Files.Get_Value (Files_Table, Name & ".gz", N, Found);

         if Found then
            Open_File (True);
         else
            File := null;
         end if;
      end if;
   end Open;

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

end AWS.Resources.Streams.Embedded;
