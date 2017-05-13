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

package body WSDL_nt is

   use Ada;

   -----------
   -- Print --
   -----------

   procedure Print
     (A : WSDL_nt_Types.Uns;
      X : WSDL_nt_Types.Values_I;
      Y : WSDL_nt_Types.Values_ID;
      Z : WSDL_nt_Types.Values_F;
      T : WSDL_nt_Types.Values_FD) is
   begin
      Text_IO.Put_Line (WSDL_nt_Types.Uns'Image (A));
      Text_IO.Put_Line (WSDL_nt_Types.Values_I'Image (X));
      Text_IO.Put_Line (WSDL_nt_Types.Values_ID'Image (Y));
      Float_Text_IO.Put (Float (Z), Fore => 1, Aft => 1, Exp => 0);
      Text_IO.New_Line;
      Float_Text_IO.Put (Float (T), Fore => 1, Aft => 1, Exp => 0);
      Text_IO.New_Line;
   end Print;

end WSDL_nt;
