------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                     Dmitriy Anisimkov - Pascal Obry                      --
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

--  Usage: agent [options] [GET/PUT] <URL>
--         -f                      force display of message body.
--         -k                      keep-alive connection.
--         -n                      non stop for stress test.
--         -proxy <proxy_url>
--         -u <user_name>
--         -p <password>
--         -pu <proxy_user_name>
--         -pp <proxy_password>
--
--  for example:
--
--     agent GET http://perso.wanadoo.fr/pascal.obry/contrib.html
--

with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Command_Line;
with Ada.Characters.Handling;
with Ada.Streams;
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
   Force      : Boolean := False;
   Keep_Alive : Boolean := False;
   Wait_Key   : Boolean := True;
   Connect    : AWS.Client.HTTP_Connection;

   procedure Parse_Command_Line;
   --  parse Agent command line:
   --  Usage: agent [-k -u -p -proxy -pu -pp] [GET/PUT] URL

   procedure Parse_Command_Line is
   begin
     loop
        case GNAT.Command_Line.Getopt ("f u: p: pu: pp: proxy: k n") is

           when ASCII.NUL =>
              exit;

           when 'f' =>
              Force := True;

           when 'n' =>
              Wait_Key := False;

           when 'k' =>
              Keep_Alive := True;

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
              raise Program_Error;         -- cannot occurs!
        end case;
     end loop;

     Method := Status.Request_Method'Value (GNAT.Command_Line.Get_Argument);
     URL    := To_Unbounded_String (GNAT.Command_Line.Get_Argument);
   end Parse_Command_Line;

   Data : Response.Data;

begin

   if Ada.Command_Line.Argument_Count = 0 then
      Text_IO.Put_Line ("Usage: agent [options] [GET/PUT] <URL>");
      Text_IO.Put_Line ("       -f           force display of message body.");
      Text_IO.Put_Line ("       -k           keep-alive connection.");
      Text_IO.Put_Line ("       -n           non stop for stress test.");
      Text_IO.Put_Line ("       -proxy <proxy_url>");
      Text_IO.Put_Line ("       -u <user_name>");
      Text_IO.Put_Line ("       -p <password>");
      Text_IO.Put_Line ("       -pu <proxy_user_name>");
      Text_IO.Put_Line ("       -pp <proxy_password>");
      return;
   end if;

   Parse_Command_Line;

   Connect := Client.Create
     (Host       => To_String (URL),
      User       => To_String (User),
      Pwd        => To_String (Pwd),
      Proxy      => To_String (Proxy),
      Proxy_User => To_String (Proxy_User),
      Proxy_Pwd  => To_String (Proxy_Pwd),
      Persistent => Keep_Alive);

   loop

      if Method = Status.GET then
         Client.Get (Connect, Data);
      else
         Client.Put (Connection => Connect,
                     Result => Data,
                     Data   => "Un essai");
      end if;

      Text_IO.Put_Line
        ("Status Code = "
         & Messages.Image (Response.Status_Code (Data))
         & " - "
         & Messages.Reason_Phrase (Response.Status_Code (Data)));

      if Response.Content_Type (Data) = "text/html" then
         Text_IO.Put_Line (Response.Message_Body (Data));

      else
         Text_IO.Put_Line
           (Messages.Content_Type (Response.Content_Type (Data)));

         Text_IO.Put_Line
           (Messages.Content_Length (Response.Content_Length (Data)));

         if Force = True then
            --  this is not a text/html body, but output it anyway

            declare
               Message_Body : constant Streams.Stream_Element_Array
                 := Response.Binary (Data);
            begin
               for K in Message_Body'Range loop
                  declare
                     C : Character := Character'Val (Message_Body (K));
                  begin
                     if C = ASCII.CR
                       or else C = ASCII.LF
                       or else not Characters.Handling.Is_Control (C)
                     then
                        Text_IO.Put (C);
                     else
                        Text_IO.Put ('.');
                     end if;
                  end;
               end loop;
            end;

         end if;
      end if;

      if Keep_Alive then
         --  check that the keep alive connection is kept alive
         if Wait_Key then

            Text_IO.Put_Line
              ("Type 'q' to exit, the connection will be closed.");

            Text_IO.Put_Line ("Any other key to retreive again the same URL");

            declare
               Char : Character;
            begin
               Text_IO.Get_Immediate (Char);
               exit when char = 'q';
            end;
         end if;

      else
         Client.Close (Connect);
         exit;
      end if;

   end loop;

end Agent;

