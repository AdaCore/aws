------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                      Copyright (C) 2017, AdaCore                         --
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

package body API_Imp is

   use Ada.Strings.Unbounded;

   procedure Call (O : API.Child_Service.Types.Rec_Type) is
   begin
      Ada.Text_IO.Put_Line ("API.Call : " & To_String (O.V));
      Ada.Text_IO.Put_Line ("         : " & O.C'Img);
      Ada.Text_IO.Put_Line ("         : " & O.D'Img);
   exception
      when others =>
         Ada.Text_IO.Put_Line ("API.Call!!!!!");
   end Call;

end API_Imp;
