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

with AWS.Client;
with AWS.Response;
with AWS.Server.Push;
with AWS.Status;

package Sp_Pck is

   use Ada;
   use AWS;

   use Ada.Text_IO;

   type Push_Data_Type is delta 0.01 digits 7;

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

   CRLF        : constant String := ASCII.CR & ASCII.LF;
   End_Of_Part : constant String := '.' & CRLF;

   function CB (Request : in Status.Data) return Response.Data;

   Connect     : array (Server_Push.Mode'Range) of Client.HTTP_Connection;
   Push        : Server_Push.Object;
   Answer      : AWS.Response.Data;

   Data        : Push_Data_Type;

   Picture : array (Server_Push.Mode) of Editing.Picture
     := (Server_Push.Plain     => Editing.To_Picture ("999999.99"),
         Server_Push.Chunked   => Editing.To_Picture ("##_##9.99"),
         Server_Push.Multipart => Editing.To_Picture ("zzzzz9.99"));

   procedure Output (Data : String);
   --  Ignore random string --AWS.Push.Boundary_1044468257,
   --  and by the way ignore ASCII.CR becouse on the Win32 platform
   --  the Ada.Text_IO.Put_Line add the ASCII.CR before ASCII.LF even so
   --  the ASCII.CR already exists before ASCII.LF.

   procedure Client_Process (Protocol : in String; Port : in Positive);

end Sp_Pck;
