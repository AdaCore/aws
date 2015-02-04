------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2015, AdaCore                        --
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
with Ada.Text_IO;

with www.ecerami.com.wsdl.deriveconstservice_wsdl.big_type_pkg;

procedure Deriveconst_Main is
   use Ada;
   use Ada.Strings.Unbounded;

   use www.ecerami.com.wsdl.deriveconstservice_wsdl.big_type_pkg;

   B1 : constant Big_Type := (1, 1, 1.0);

begin
   Text_IO.Put_Line ("Run OK");
end Deriveconst_Main;
