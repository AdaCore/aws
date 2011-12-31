------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2012, AdaCore                     --
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

with Ada.Strings.Unbounded;

package Framework is

   use Ada.Strings.Unbounded;

   type Option_Data is record
      Short_Name  : Unbounded_String;
      Full_Name   : Unbounded_String;
      Description : Unbounded_String;
   end record;

   type Rank_Data is record
      R_Name : Unbounded_String;
      Count  : Integer;
   end record;

   type Options_Table is array (Positive range <>) of Option_Data;

   type Files_Table is array (Positive range <>) of Unbounded_String;

   type Ranks_Table is array (1 .. 15) of Framework.Rank_Data;

end Framework;
