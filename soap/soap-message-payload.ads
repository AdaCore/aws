------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimkov - Pascal Obry                                --
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

--  $Id$

with SOAP.Parameters;

package SOAP.Message.Payload is

   type Object is new Message.Object with private;

   function Procedure_Name (P : in Object'Class) return String;
   --  Retruns the Payload procedure name.

   procedure Set_Procedure_Name (P : in out Object'Class; Name : in String);
   --  Set the payload procedure name.

   function Build
     (Procedure_Name : in String;
      P_Set          : in SOAP.Parameters.List;
      Name_Space     : in String               := "")
     return Object;
   --  Retruns a Payload object initialized with the procedure name,
   --  parameters and name space.

private

   type Object is new Message.Object with null record;

end SOAP.Message.Payload;
