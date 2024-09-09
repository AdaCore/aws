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

with AWS.Utils;

with Ada.Text_IO;

package body WSDL_D is

   use Ada;
   use AWS;

   ----------
   -- Call --
   ----------

   procedure Call (O : R) is
   begin
      Text_IO.Put_Line (O.D'Img);
   end Call;

   -----------
   -- Print --
   -----------

   procedure Print (D : SOAP.Types.Decimal) is
   begin
      Text_IO.Put_Line (D'Img);
   end Print;

   -----------------
   -- Print_Price --
   -----------------

   procedure Print_Price (P : Price) is
   begin
      Text_IO.Put_Line (P'Img);
   end Print_Price;

   -----------
   -- Image --
   -----------

   function Image (D : SOAP.Types.Decimal) return String is
   begin
      return D'Img;
   end Image;

end WSDL_D;
