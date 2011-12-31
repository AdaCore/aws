------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
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
     (Resource       : in out AWS.Resources.Streams.Stream_Type'Class;
      Size           : Stream_Element_Offset;
      Undefined_Size : Boolean) is
   begin
      File_Tagged (Resource).Undefined_Size := Undefined_Size;
      File_Tagged (Resource).Size           := Size;
      File_Tagged (Resource).Offset         := 0;
   end Create;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File
     (Resource : File_Tagged) return Boolean is
   begin
      return Resource.Offset >= Resource.Size;
   end End_Of_File;

   ----------
   -- Read --
   ----------

   procedure Read
     (Resource : in out File_Tagged;
      Buffer   : out Stream_Element_Array;
      Last     : out Stream_Element_Offset)
   is
      Symbol_First  : constant Character := '!';
      Symbol_Last   : constant Character := '~';
      Symbol_Length : constant Stream_Element
        := Character'Pos (Symbol_Last) - Character'Pos (Symbol_First) + 1;

      Item          : Stream_Element;

   begin
      --  !!! Do not change the way of the data for read.
      --  It is just for control error when only CRC in the last chunk
      --  of the deflate compressed data.

      Last := Buffer'First - 1;

      for I in Buffer'Range loop
         exit when End_Of_File (Resource);
         Last := I;

         Resource.Offset := Resource.Offset + 1;

         Item := Stream_Element
           (Resource.Offset mod Stream_Element_Offset (Stream_Element'Last));

         Buffer (I) := Character'Pos (Symbol_First)
           + Item mod Symbol_Length;

         if Resource.Offset mod 80 = 0 then
            Buffer (I) := 10;
         end if;

         if Resource.Offset mod 81 = 0 then
            Buffer (I) := 9;
         end if;
      end loop;
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

   ----------
   -- Size --
   ----------

   function Size (File : File_Tagged) return Stream_Element_Offset is
   begin
      if File.Undefined_Size then
         return AWS.Resources.Undefined_Length;
      else
         return File.Size;
      end if;
   end Size;

end User_Strm;
