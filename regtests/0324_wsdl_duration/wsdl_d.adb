------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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
      Text_IO.Put_Line (Utils.Significant_Image (O.D, 6));
   end Call;

   -----------
   -- Print --
   -----------

   procedure Print (X : Duration) is
   begin
      Text_IO.Put_Line (Utils.Significant_Image (X, 6));
   end Print;

   -----------
   -- Image --
   -----------

   function Image (D : Duration) return String is
   begin
      return Utils.Significant_Image (D, 6);
   end Image;

end WSDL_D;
