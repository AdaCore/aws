------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2017, AdaCore                     --
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

package body AWS.Resources.Streams.Disk is

   -----------
   -- Close --
   -----------

   overriding procedure Close (Resource : in out Stream_Type) is
   begin
      if Stream_IO.Is_Open (Resource.File) then
         Stream_IO.Close (Resource.File);
      end if;
   end Close;

   -----------------
   -- End_Of_File --
   -----------------

   overriding function End_Of_File
     (Resource : Stream_Type) return Boolean is
   begin
      return Resource.Current > Resource.Last
        and then Stream_IO.End_Of_File (Resource.File);
   end End_Of_File;

   ----------
   -- Name --
   ----------

   overriding function Name (Resource : Stream_Type) return String is
   begin
      return To_String (Resource.Name);
   end Name;

   ----------
   -- Open --
   ----------

   procedure Open
     (File : out Stream_Type;
      Name : String;
      Form : String    := "shared=no") is
   begin
      File.Name := To_Unbounded_String (Name);

      Stream_IO.Open
        (File.File,
         Stream_IO.In_File, Name, Form);
      File.Stream := Stream_IO.Stream (File.File);
   exception
      when Stream_IO.Name_Error =>
         null;
   end Open;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Resource : in out Stream_Type;
      Buffer   : out Stream_Element_Array;
      Last     : out Stream_Element_Offset)
   is
      Buf_Len : constant Stream_Element_Offset :=
                  Resource.Last - Resource.Current + 1;
   begin
      if Buffer'Length <= Natural (Buf_Len) then
         --  Enough chars in the buffer, return them
         Buffer := Resource.Buffer
           (Resource.Current .. Resource.Current + Buffer'Length - 1);
         Resource.Current := Resource.Current + Buffer'Length;
         Last := Buffer'Last;

      else
         --  Return the current buffer

         Buffer (Buffer'First .. Buffer'First + Buf_Len - 1) :=
           Resource.Buffer (Resource.Current .. Resource.Last);

         --  And read the remaining data directly on the file

         Read (Resource.Stream.all,
               Buffer (Buffer'First + Buf_Len .. Buffer'Last),
               Last);

         Resource.Current := Resource.Buffer'First;

         if Last < Buffer'Last then
            --  There is no more data, set the Resource object
            Resource.Last := 0;

         else
            --  Fill Resource buffer
            Read (Resource.Stream.all, Resource.Buffer, Resource.Last);
         end if;
      end if;
   end Read;

   -----------
   -- Reset --
   -----------

   overriding procedure Reset (Resource : in out Stream_Type) is
   begin
      Stream_IO.Reset (Resource.File);
   end Reset;

   ---------------
   -- Set_Index --
   ---------------

   overriding procedure Set_Index
     (Resource : in out Stream_Type;
      To       : Stream_Element_Offset)
   is
      Size : constant Stream_Element_Offset :=
               Stream_Element_Offset (Stream_IO.Size (Resource.File));
      Pos  : Stream_Element_Offset := To;
   begin
      if To < 1 or else To > Size then
         Pos := Size + 1;
      end if;

      Stream_IO.Set_Index (Resource.File, Stream_IO.Count (Pos));
   end Set_Index;

   ----------
   -- Size --
   ----------

   overriding function Size
     (Resource : Stream_Type) return Stream_Element_Offset is
   begin
      return Stream_Element_Offset (Stream_IO.Size (Resource.File));
   end Size;

end AWS.Resources.Streams.Disk;
