------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2002                          --
--                               ACT-Europe                                 --
--                                                                          --
--  Authors: Dmitriy Anisimokv - Pascal Obry                                --
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

package body User_Strm is

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
      return Boolean
   is
   begin
      return Resource.Offset >= Resource.Size;
   end End_Of_File;

   ----------
   -- Read --
   ----------

   procedure Read
     (Resource : in out File_Tagged;
      Buffer   :    out Stream_Element_Array;
      Last     :    out Stream_Element_Offset)
   is
      Symbol_First  : constant Character := '0';
      Symbol_Last   : constant Character := 'z';
      Symbol_Length : constant Stream_Element
        := Character'Pos (Symbol_Last) - Character'Pos (Symbol_First) + 1;
   begin
      Last := Buffer'First - 1;

      for I in Buffer'Range loop
         exit when End_Of_File (Resource);
         Last := I;

         Resource.Offset := Resource.Offset + 1;
         Buffer (I) := Character'Pos (Symbol_First)
            + Stream_Element (Resource.Offset) mod Symbol_Length;

         if Stream_Element (Resource.Offset)
            mod (Symbol_Length - 1) = 0
         then
            Buffer (I) := 10;
         end if;

      end loop;
   end Read;

end User_Strm;
