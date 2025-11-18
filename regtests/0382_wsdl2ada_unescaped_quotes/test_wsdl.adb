------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2025, AdaCore                        --
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

--  SOAP/WSDL test

--  Checks that `"` from `<xs:pattern value` properly handled to create
--  a valid pattern string for regexp

with Ada.Text_IO;

--  Just make sure that the generated code is OK and can be compiled

with www.ecerami.com.wsdl.DeriveconstService_wsdl.Code1_Type_Pkg;

procedure Test_Wsdl is
begin
   Ada.Text_IO.Put_Line ("Compiled and run fine");
end Test_Wsdl;
