------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2024, AdaCore                        --
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

package body WSDL_D is

   use Ada;
   use Ada.Strings.Unbounded;

   ----------
   -- Call --
   ----------

   procedure Call
     (V : String;
      T : SOAP.Types.Token;
      N : SOAP.Types.Normalized_String;
      U : SOAP.Types.Any_URI) is
   begin
      Text_IO.Put_Line ("Value:             (" & V & ')');
      Text_IO.Put_Line ("Token:             (" & T & ')');
      Text_IO.Put_Line ("Normalized_String: (" & N & ')');
      Text_IO.Put_Line ("Any_URI:           (" & U & ')');
   end Call;

end WSDL_D;
