------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                            Copyright (C) 2002                            --
--                               Pascal Obry                                --
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

package Templates_Parser.Input is

   type File_Type is limited private;

   procedure Open
     (File : in out File_Type;
      Name : in String;
      Form : in String := "");
   pragma Inline (Open);
   --  Like Text_IO.Open and Mode = In_File.

   procedure Close (File : in out File_Type);
   pragma Inline (Close);
   --  Like Text_IO.Close. Raises text_IO.Status_Error is file is not open.

   function End_Of_File (File : in File_Type) return Boolean;
   pragma Inline (End_Of_File);
   --  Like Text_IO.End_Of_File. Raises Text_IO.Status_Error is file is not
   --  open.

   procedure Get_Line
     (File   : in     File_Type;
      Buffer :    out String;
      Last   :    out Natural);
   pragma Inline (Get_Line);
   --  Like Text_IO.Get_Line. Raises Text_IO.Status_Error is file is not open.

private

   type File_Record;
   type File_Type is access File_Record;

end Templates_Parser.Input;
