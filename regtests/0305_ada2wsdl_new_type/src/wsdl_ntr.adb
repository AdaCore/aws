------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2017, AdaCore                        --
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

with Ada.Float_Text_IO;
with Ada.Text_IO;

package body WSDL_ntr is

   use Ada;

   -----------
   -- Print --
   -----------

   procedure Print (R : WSDL_ntr_Types.Rec) is
   begin
      Text_IO.Put_Line (WSDL_ntr_Types.Values_I'Image (R.V1));
      Float_Text_IO.Put (Float (R.V2), Fore => 1, Aft => 1, Exp => 0);
      Text_IO.New_Line;
   end Print;

end WSDL_ntr;
