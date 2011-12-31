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

with AWS.Templates;

package Web_Elements_Containers is

   procedure Add_Group (Name : String);
   --  Add a new group

   function Get_Groups return AWS.Templates.Vector_Tag;
   --  Returns the list of groups as a vector tag

   procedure Add_User (Name : String);
   --  Add a new user

   function Get_Users return AWS.Templates.Vector_Tag;
   --  Returns the list of users as a vector tag

end Web_Elements_Containers;
