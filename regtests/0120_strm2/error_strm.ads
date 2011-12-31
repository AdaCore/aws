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

--  Test for user defined stream raising an exception

with Ada.Streams;

with AWS.Resources.Streams;

package Error_Strm is

   use AWS.Resources;
   use Ada.Streams;

   type File_Tagged is new Streams.Stream_Type with private;

   function End_Of_File
     (Resource : File_Tagged)
      return Boolean;

   procedure Read
     (Resource : in out File_Tagged;
      Buffer   : out Stream_Element_Array;
      Last     : out Stream_Element_Offset);

   procedure Close (File : in out File_Tagged);

   procedure Reset (File : in out File_Tagged);

   procedure Set_Index
     (File     : in out File_Tagged;
      Position : Stream_Element_Offset);

   procedure Create
     (Resource : in out AWS.Resources.Streams.Stream_Type'Class;
      Size     : Stream_Element_Offset);

private

   type File_Tagged is new Streams.Stream_Type with record
      Offset : Stream_Element_Offset;
      Size   : Stream_Element_Offset;
   end record;

end Error_Strm;
