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

pragma Ada_2022;

with Ada.Text_IO;

with RecFieldsService.Client;
with RecFieldsService.Server;
with RecFieldsService.Types;
with simple.doc.net.sd.Exampletype_Type_Pkg;

procedure RecFields is

   use Ada;

   use simple.doc.net.sd.Exampletype_Type_Pkg;

   R : n1_ExampleType_Type;

begin
   R := ([1], 2, 3);
   Text_IO.Put_Line ("OK");
end RecFields;
