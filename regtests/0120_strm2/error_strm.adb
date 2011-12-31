------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
--                                                                          --
--  This is free software;  you can redistribute it  and/or modify it       --
--  under terms of the  GNU General Public License as published  by the     --
--  Free Software  Foundation;  either version 3,  or (at your option) any  --
--  later version.  This software is distributed in the hope  that it will  --
--  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU     --
--  General Public License for  more details.                               --
--                                                                          --
--  You should have  received  a copy of the GNU General  Public  License   --
--  distributed  with  this  software;   see  file COPYING3.  If not, go    --
--  to http://www.gnu.org/licenses for a complete copy of the license.      --
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
      Size     : Stream_Element_Offset) is
   begin
      File_Tagged (Resource).Size   := Size;
      File_Tagged (Resource).Offset := 0;
   end Create;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File
     (Resource : File_Tagged)
      return Boolean is
   begin
      return Resource.Offset >= Resource.Size;
   end End_Of_File;

   ----------
   -- Read --
   ----------

   procedure Read
     (Resource : in out File_Tagged;
      Buffer   : out Stream_Element_Array;
      Last     : out Stream_Element_Offset) is
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
      Position : Stream_Element_Offset) is
   begin
      null;
   end Set_Index;

end Error_Strm;
