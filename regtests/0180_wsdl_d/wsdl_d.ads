------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                    Copyright (C) 2008-2025, AdaCore                      --
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

with Ada.Containers.Vectors;
with SOAP.Utils;

package WSDL_D is

   use SOAP;

   package Name_Set_Pkg is
     new Ada.Containers.Vectors (Positive, Integer);

   package Name_Set1_Pkg is
     new Ada.Containers.Vectors (Positive, Integer);

   package Name_Set2_Pkg is
     new Ada.Containers.Vectors (Positive, Integer);

end WSDL_D;
