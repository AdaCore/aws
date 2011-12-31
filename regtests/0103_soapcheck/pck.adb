------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
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

with Ada.Integer_Text_IO;
with Ada.Text_IO;

package body Pck is

   use Ada;

   procedure Call (R : Rec) is
   begin
      Integer_Text_IO.Put (R.V);
      Text_IO.New_Line;
   end Call;

   procedure Print (X : Integer; Name : String) is
   begin
      Integer_Text_IO.Put (X);
      Text_IO.Put_Line (Name);
   end Print;

   procedure Big (R : R_Rec) is
   begin
      null;
   end Big;

end Pck;
