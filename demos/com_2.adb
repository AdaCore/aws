------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2000                            --
--                      Dmitriy Anisimkov & Pascal Obry                     --
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

--  Com_1 and Com_2 are two demos programs which are using the AWS
--  communication protocol. You must first launch Com_1 then Com_2.

with Ada.Text_IO;
with Ada.Strings.Unbounded;

with AWS.Communication.Client;
with AWS.Communication.Server;
with AWS.Response;
with AWS.Utils;

procedure Com_2 is

   use Ada;
   use Ada.Strings.Unbounded;
   use AWS;

   N : Natural := 0;

   -------------
   -- Receive --
   -------------

   function Receive
     (Server     : in String;
      Message    : in String;
      Parameters : in Communication.Parameter_Set
        := Communication.Null_Parameter_Set)
     return Response.Data is
   begin
      Text_IO.Put_Line ("Server " & Server
                        & " send me the message " & Message);

      for K in Parameters'Range loop
         Text_IO.Put_Line ("   P" & Utils.Image (K) & " = "
                           & To_String (Parameters (K)));
      end loop;
      Text_IO.New_Line;

      N := N + 1;
      return Response.Build ("text/html", "Ans [" & Utils.Image (N) & ']');
   end Receive;

   package Local_Server is new Communication.Server (3456, Receive);

   Answer : Response.Data;

begin
   for K in 1 .. 10 loop
      Answer := Communication.Client.Send_Message
        ("localhost", 1234, "mes2." & Utils.Image (K));
      Text_IO.Put_Line ("< reply " & Response.Message_Body (Answer));
   end loop;
end Com_2;
