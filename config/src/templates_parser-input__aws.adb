------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                      Copyright (C) 2002-2013, AdaCore                    --
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

pragma Ada_2012;

--  This is the implementation to be used with AWS, it is using AWS.Resources
--  to support embedded resources.

with Ada.Unchecked_Deallocation;

with AWS.Resources;

package body Templates_Parser.Input is

   type File_Record is new AWS.Resources.File_Type;

   procedure Check_Open (File : File_Type) with Inline;
   --  Check if File is opened (File variable is not null)

   procedure Free is new Ada.Unchecked_Deallocation (File_Record, File_Type);

   ----------------
   -- Check_Open --
   ----------------

   procedure Check_Open (File : File_Type) is
   begin
      if File = null then
         raise Status_Error;
      end if;
   end Check_Open;

   -----------
   -- Close --
   -----------

   procedure Close (File : in out File_Type) is
   begin
      Check_Open (File);
      Close (File.all);
      Free (File);
   end Close;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File (File : File_Type) return Boolean is
   begin
      Check_Open (File);
      return End_Of_File (File.all);
   end End_Of_File;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line
     (File   : File_Type;
      Buffer : out String;
      Last   : out Natural) is
   begin
      Check_Open (File);
      Get_Line (File.all, Buffer, Last);
   end Get_Line;

   -------------------
   -- LF_Terminated --
   -------------------

   function LF_Terminated (File : File_Type) return Boolean is
   begin
      Check_Open (File);
      return LF_Terminated (File.all);
   end LF_Terminated;

   ----------
   -- Open --
   ----------

   procedure Open
     (File : in out File_Type;
      Name : String;
      Form : String    := "") is
   begin
      if File /= null then
         Close (File);
      end if;

      File := new File_Record;
      Open (File.all, Name, Form);
   exception
      when IO_Exceptions.Name_Error =>
         Free (File);
         raise Name_Error with "File '" & Name & "' not found.";
   end Open;

end Templates_Parser.Input;
