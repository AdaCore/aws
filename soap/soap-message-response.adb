------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2008, AdaCore                     --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

with AWS.MIME;

with SOAP.Message.XML;

package body SOAP.Message.Response is

   -----------
   -- Build --
   -----------

   function Build (R : in Object'Class) return AWS.Response.Data is
   begin
      return AWS.Response.Build
         (AWS.MIME.Text_XML, UString_Message => SOAP.Message.XML.Image (R));
   end Build;

   ----------
   -- From --
   ----------

   function From (P : in Message.Payload.Object) return Object is
      NP : Object;
   begin
      Set_Wrapper_Name (NP, Payload.Procedure_Name (P) & "Response");

      Set_Parameters   (NP, Parameters (P));

      Set_Name_Space (NP, Name_Space (P));

      return NP;
   end From;

   --------------
   -- Is_Error --
   --------------

   function Is_Error (R : in Object) return Boolean is
      pragma Warnings (Off, R);
   begin
      return False;
   end Is_Error;

end SOAP.Message.Response;
