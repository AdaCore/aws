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

with AWS.OS_Lib;

package body AWS.Resources.Files is

   -----------
   -- Close --
   -----------

   procedure Close (Resource : in out File_Tagged) is
   begin
      Stream_IO.Close (Resource.File);
   end Close;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File (Resource : in File_Tagged) return Boolean is
   begin
      return Resource.Current > Resource.Last
        and then Stream_IO.End_Of_File (Resource.File);
   end End_Of_File;

   ---------------
   -- File_Size --
   ---------------

   function File_Size
     (Name : in String)
      return Ada.Streams.Stream_Element_Offset is
   begin
      return OS_Lib.File_Size (Name);
   exception
      when others =>
         raise Resource_Error;
   end File_Size;

   --------------------
   -- File_Timestamp --
   --------------------

   function File_Timestamp (Name : in String) return Ada.Calendar.Time is
   begin
      return OS_Lib.File_Timestamp (Name);
   exception
      when others =>
         raise Resource_Error;
   end File_Timestamp;

   ---------------------
   -- Is_Regular_File --
   ---------------------

   function Is_Regular_File (Name : in String) return Boolean is
   begin
      return OS_Lib.Is_Regular_File (Name);
   exception
      when others =>
         raise Resource_Error;
   end Is_Regular_File;

   ----------
   -- Open --
   ----------

   procedure Open
     (File :    out File_Type;
      Name : in     String;
      Form : in     String    := "") is
   begin
      File := new File_Tagged;

      Stream_IO.Open
        (File_Tagged (File.all).File,
         Stream_IO.In_File, Name, Form);

      File_Tagged (File.all).Stream :=
        Stream_IO.Stream (File_Tagged (File.all).File);
   end Open;

   ----------
   -- Read --
   ----------

   procedure Read
     (Resource : in out File_Tagged;
      Buffer   :    out Stream_Element_Array;
      Last     :    out Stream_Element_Offset)
   is
      use type Stream_Element_Offset;

      Buf_Len : constant Natural
        := Integer (Resource.Last - Resource.Current) + 1;
   begin
      if Buffer'Length <= Buf_Len then
         --  Enough chars in the buffer, return them
         Buffer := Resource.Buffer
           (Resource.Current .. Resource.Current + Buffer'Length - 1);
         Resource.Current := Resource.Current + Buffer'Length;
         Last := Buffer'Last;

      else
         --  Return the current buffer
         Buffer
           (Buffer'First .. Buffer'First + Stream_Element_Offset (Buf_Len) - 1)
           := Resource.Buffer (Resource.Current .. Resource.Last);

         --  And read the remaining data on the file
         Read
           (Resource.Stream.all,
            Buffer
              (Buffer'First + Stream_Element_Offset (Buf_Len) .. Buffer'Last),
            Last);

         Read (Resource.Stream.all, Resource.Buffer, Resource.Last);
         Resource.Current := Resource.Buffer'First;
      end if;
   end Read;

end AWS.Resources.Files;
