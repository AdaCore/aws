------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2008, AdaCore                     --
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

package body Error_Strm is

   -----------
   -- Close --
   -----------

   procedure Close (File : in out File_Tagged) is
   begin
      null;
   end Close;

   ------------
   -- Create --
   ------------

   procedure Create
     (Resource : in out AWS.Resources.Streams.Stream_Type'Class;
      Size     : in     Stream_Element_Offset) is
   begin
      File_Tagged (Resource).Size   := Size;
      File_Tagged (Resource).Offset := 0;
   end Create;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File
     (Resource : in File_Tagged)
      return Boolean is
   begin
      return Resource.Offset >= Resource.Size;
   end End_Of_File;

   ----------
   -- Read --
   ----------

   procedure Read
     (Resource : in out File_Tagged;
      Buffer   :    out Stream_Element_Array;
      Last     :    out Stream_Element_Offset) is
   begin
      raise Constraint_Error;
   end Read;

   -----------
   -- Reset --
   -----------

   procedure Reset (File : in out File_Tagged) is
   begin
      null;
   end Reset;

   ---------------
   -- Set_Index --
   ---------------

   procedure Set_Index
     (File     : in out File_Tagged;
      Position : in     Stream_Element_Offset) is
   begin
      null;
   end Set_Index;

end Error_Strm;
