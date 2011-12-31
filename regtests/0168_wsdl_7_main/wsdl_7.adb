------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2012, AdaCore                     --
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

with Ada.Text_IO;
with SOAP.Types;

package body WSDL_7 is

   use Ada;
   use SOAP.Types;

   Keep : SOAP.Types.SOAP_Base64;

   ----------
   -- Proc --
   ----------

   procedure Proc (P : SOAP.Utils.SOAP_Base64) is
   begin
      Keep := Types.B64 (P, "P");
   end Proc;

   ----------
   -- Func --
   ----------

   function Func (P : Integer) return Utils.SOAP_Base64 is
   begin
      Text_IO.Put_Line (Integer'Image (P));
      return V (Keep);
   end Func;

end WSDL_7;
