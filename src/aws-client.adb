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

with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;

with Sockets;

package body AWS.Client is

   use Ada;
   use Ada.Strings;
   use Ada.Strings.Unbounded;

   HTTP_Token : constant String := "http://";
   subtype HTTP_Range is Positive range HTTP_Token'Range;

   End_Section : constant String := "";
   CRLF        : constant String := ASCII.CR & ASCII.LF;
   CRLF_Size   : constant        := 2;

   ---------
   -- Get --
   ---------

   function Get (URL : in String) return Response.Data is

      I1 : Natural; --  start index for the server name
      I2 : Natural; --  start index for the port number
      I3 : Natural; --  start index for the URI

      procedure Cut_URL;
      --  parse URL and set I1 and I2. Raises URL_Error if the URL is not well
      --  writen.

      function Server return String;
      --  return server name from URL. Cut_URL must have been called before.

      function Port return Positive;
      --  return port number from URL. Cut_URL must have been called before.

      function URI return String;
      --  return URI from URL. Cut_URL must have been called before.

      function Image (P : in Positive) return String;
      --  return P image without leading space

      Sock    : Sockets.Socket_FD;
      CT      : Unbounded_String;
      CT_Len  : Natural;
      Message : Unbounded_String;
      M_Len   : Natural := 0;

      -------------
      -- Cut_URL --
      -------------

      procedure Cut_URL is
      begin
         if URL'Length < HTTP_Range'Last
           or else URL (HTTP_Range) /= HTTP_Token
         then
            raise URL_Error;
         else
            I1 := HTTP_Range'Last + 1;
            I2 := Fixed.Index (URL (HTTP_Range'Last + 1 .. URL'Last), ":");
            I3 := Fixed.Index (URL (HTTP_Range'Last + 1 .. URL'Last), "/");
         end if;
      end Cut_URL;

      ------------
      -- Server --
      ------------

      function Server return String is
      begin
         if I2 = 0 then
            if I3 = 0 then
               return URL (I1 .. URL'Last);
            else
               return URL (I1 .. I3 - 1);
            end if;
         else
            return URL (I1 .. I2 - 1);
         end if;
      end Server;

      ----------
      -- Port --
      ----------

      function Port return Positive is
      begin
         if I2 = 0 then
            return 80;
         else
            if I3 = 0 then
               return Positive'Value (URL (I2 + 1 .. URL'Last));
            else
               return Positive'Value (URL (I2 + 1 .. I3 - 1));
            end if;
         end if;
      end Port;

      ---------
      -- URI --
      ---------

      function URI return String is
      begin
         if I3 = 0 then
            return "/";
         else
            return URL (I3 .. URL'Last);
         end if;
      end URI;

      -----------
      -- Image --
      -----------

      function Image (P : in Positive) return String is
         PI : constant String := Positive'Image (P);
      begin
         return PI (2 .. PI'Last);
      end Image;

   begin

      Cut_URL;

      -- Connect to server

      Sock := Sockets.Socket (Sockets.AF_INET, Sockets.SOCK_STREAM);

      Text_IO.Put_Line ("Connect to " & Server & Positive'Image (Port));

      Sockets.Connect (Sock, Server, Port);

      Text_IO.Put_Line ("Send request...");

      Sockets.Put_Line (Sock, "GET " & URI & ' ' & HTTP_Version);
      Sockets.Put_Line (Sock, "Accept: text/html");
      Sockets.Put_Line (Sock, "Accept-Language: fr, us");
      Sockets.Put_Line (Sock, "User-Agent: AWS v" & Version);
      Sockets.Put_Line (Sock, "Host: " & Server & ':' & Image (Port));
      Sockets.Put_Line (Sock, "Connection: Keep-Alive");
      Sockets.New_Line (Sock);

      Parse_Header : loop
         declare
            Line : constant String := Sockets.Get_Line (Sock);
         begin
            Text_IO.Put_Line ("H=" & Line);
            if Line = End_Section then
               exit Parse_Header;

            elsif Messages.Is_Match (Line, Messages.Content_Type_Token) then
               CT := To_Unbounded_String
                 (Line (Messages.Content_Type_Token'Last + 1 .. Line'Last));

            elsif Messages.Is_Match (Line, Messages.Content_Length_Token) then
               CT_Len := Natural'Value
                 (Line (Messages.Content_Length_Range'Last + 1 .. Line'Last));
            else
               --  everything else is ignore right now
               null;
            end if;
         end;
      end loop Parse_Header;

      Parse_Body : loop
         declare
            Line : constant String := Sockets.Get_Line (Sock);
         begin
            Text_IO.Put_Line ("B=" & Line);
            M_Len := M_Len + Line'Length + CRLF_Size;

            Message := Message & Line & CRLF;

            exit when M_Len = CT_Len;
         end;
      end loop Parse_Body;

      Sockets.Shutdown (Sock);

      return Response.Build (To_String (CT),
                             To_String (Message));
   exception
      when others =>
         raise URL_Error;
   end Get;

end AWS.Client;
