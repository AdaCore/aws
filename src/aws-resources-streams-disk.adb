------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2003                            --
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

package body AWS.Resources.Streams.Disk is

   -----------
   -- Close --
   -----------

   procedure Close (Resource : in out Stream_Type) is
   begin
      Stream_IO.Close (Resource.File);
   end Close;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File (Resource : in Stream_Type) return Boolean is
   begin
      return Resource.Current > Resource.Last
        and then Stream_IO.End_Of_File (Resource.File);
   end End_Of_File;

   ----------
   -- Name --
   ----------

   function Name (Resource : in Stream_Type) return String is
   begin
      return Stream_IO.Name (Resource.File);
   end Name;

   ----------
   -- Open --
   ----------

   procedure Open
     (File :    out Stream_Type;
      Name : in     String;
      Form : in     String    := "") is
   begin
      Stream_IO.Open
        (File.File,
         Stream_IO.In_File, Name, Form);

      File.Stream := Stream_IO.Stream (File.File);
   end Open;

   ----------
   -- Read --
   ----------

   procedure Read
     (Resource : in out Stream_Type;
      Buffer   :    out Stream_Element_Array;
      Last     :    out Stream_Element_Offset)
   is
      use type Stream_Element_Offset;

      Buf_Len : constant Stream_Element_Offset
        := Resource.Last - Resource.Current + 1;
   begin
      if Buffer'Length <= Natural (Buf_Len) then
         --  Enough chars in the buffer, return them
         Buffer := Resource.Buffer
           (Resource.Current .. Resource.Current + Buffer'Length - 1);
         Resource.Current := Resource.Current + Buffer'Length;
         Last := Buffer'Last;

      else
         --  Return the current buffer

         Buffer (Buffer'First .. Buffer'First + Buf_Len - 1)
           := Resource.Buffer (Resource.Current .. Resource.Last);

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

   procedure Reset (Resource : in out Stream_Type) is
   begin
      Stream_IO.Reset (Resource.File);
   end Reset;

   ----------
   -- Size --
   ----------

   function Size (Resource : in Stream_Type) return Stream_Element_Offset is
   begin
      return Stream_Element_Offset (Stream_IO.Size (Resource.File));
   end Size;

end AWS.Resources.Streams.Disk;
