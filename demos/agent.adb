------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2000                            --
--                               Pascal Obry                                --
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

--  usage : agent URL

with Ada.Text_IO;
with Ada.Strings.Unbounded;
with GNAT.Command_Line;

with AWS.Client;
with AWS.Response;
with AWS.Messages;
with AWS.Status;

procedure Agent is

   use AWS;
   use Ada;
   use Ada.Strings.Unbounded;
   use type Status.Request_Method;

   Method     : Status.Request_Method;
   User       : Unbounded_String;
   Pwd        : Unbounded_String;
   URL        : Unbounded_String;
   Proxy      : Unbounded_String;
   Proxy_User : Unbounded_String;
   Proxy_Pwd  : Unbounded_String;

   procedure Parse_Command_Line;
   --  parse Agent command line:
   --  Usage: agent [-u -p -proxy -pu -pp] [GET/PUT] URL

   procedure Parse_Command_Line is
   begin
     loop
        case GNAT.Command_Line.Getopt ("u: p: pu: pp: proxy:") is

           when ASCII.NUL =>
              exit;

           when 'u' =>
              User := To_Unbounded_String (GNAT.Command_Line.Parameter);

           when 'p' =>
              if GNAT.Command_Line.Full_Switch = "p" then
                 Pwd := To_Unbounded_String (GNAT.Command_Line.Parameter);

              elsif GNAT.Command_Line.Full_Switch = "pu" then
                 Proxy_User :=
                   To_Unbounded_String (GNAT.Command_Line.Parameter);

              elsif GNAT.Command_Line.Full_Switch = "pp" then
                 Proxy_Pwd :=
                   To_Unbounded_String (GNAT.Command_Line.Parameter);

              elsif GNAT.Command_Line.Full_Switch = "proxy" then
                 Proxy :=
                   To_Unbounded_String (GNAT.Command_Line.Parameter);
              end if;

           when others =>
              raise Program_Error;         -- cannot occur!
        end case;
     end loop;

     Method := Status.Request_Method'Value (GNAT.Command_Line.Get_Argument);
     URL    := To_Unbounded_String (GNAT.Command_Line.Get_Argument);
   end Parse_Command_Line;

   Data : Response.Data;

begin
   Parse_Command_Line;

   if Method = Status.GET then
      Data := Client.Get (URL        => To_String (URL),
                          User       => To_String (User),
                          Pwd        => To_String (Pwd),
                          Proxy      => To_String (Proxy),
                          Proxy_User => To_String (Proxy_User),
                          Proxy_Pwd  => To_String (Proxy_Pwd));
   else
      Data := Client.Put (URL        => To_String (URL),
                          Data       => "Un essai",
                          User       => To_String (User),
                          Pwd        => To_String (Pwd),
                          Proxy      => To_String (Proxy),
                          Proxy_User => To_String (Proxy_User),
                          Proxy_Pwd  => To_String (Proxy_Pwd));
   end if;

   Text_IO.Put_Line
     ("Status Code = "
      & Messages.Image (Response.Status_Code (Data))
      & " - "
      & Messages.Reason_Phrase (Response.Status_Code (Data)));

   if Response.Content_Type (Data) = "text/html" then
      Text_IO.Put_Line (Response.Message_Body (Data));
   else
      Text_IO.Put_Line ("Content-Type: "
                        & Response.Content_Type (Data));
      Text_IO.Put_Line ("Content-Length: "
                        & Natural'Image (Response.Content_Length (Data)));
   end if;
end Agent;
