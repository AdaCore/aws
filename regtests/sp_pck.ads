------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2004                            --
--                               ACT-Europe                                 --
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

--  Server push regression test.

with Ada.Strings.Fixed;
with Ada.Text_IO.Editing;

with AWS.Server.Push;

package Sp_Pck is

   use Ada;

   type Push_Data_Type is delta 0.01 digits 7;

   CRLF        : constant String := ASCII.CR & ASCII.LF;
   End_Of_Part : constant String := '.' & CRLF;

   function Image
     (Data : in Push_Data_Type;
      Env  : in Text_IO.Editing.Picture)
      return String;

   package Server_Push is new AWS.Server.Push
     (Client_Output_Type => Push_Data_Type,
      Stream_Output_Type => String,
      Client_Environment => Text_IO.Editing.Picture,
      To_Stream_Output   => Image);

   package Format is new Text_IO.Editing.Decimal_Output (Push_Data_Type);

end Sp_Pck;
