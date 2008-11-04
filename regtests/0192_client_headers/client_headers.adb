------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2008, AdaCore                        --
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

with Ada.Text_IO;
with Ada.Streams;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;

with AWS.Client;
with AWS.Headers.Set;
with AWS.Messages;
with AWS.MIME;
with AWS.Net.Log;
with AWS.Parameters;
with AWS.Response;
with AWS.Server;
with AWS.Status;
with AWS.Translator;
with AWS.Utils;

with Get_Free_Port;

procedure Client_Headers is

   use Ada;
   use Ada.Streams;
   use AWS;

   WS   : Server.HTTP;
   Port : Positive := 8270;

   --------
   -- CB --
   --------

   function CB (Request : in Status.Data) return Response.Data is
      H : constant Headers.List := Status.Header (Request);

      procedure Output (Header : in String);
      --  Output corresping header value

      ------------
      -- Output --
      ------------

      procedure Output (Header : in String) is
      begin
         if Headers.Exist (H, Header) then
            Text_IO.Put_Line
              (Header & ": " &
               Strings.Fixed.Translate
                 (Headers.Get (H, Header),
                  Strings.Maps.To_Mapping ("0123456789", "xxxxxxxxxx")));
         else
            Text_IO.Put_Line (Header & ": NOT FOUND");
         end if;
      end Output;

   begin
      Text_IO.Put_Line (">>>>> " & Status.URI (Request));
      Output (Messages.User_Agent_Token);
      Output (Messages.Accept_Language_Token);
      Output (Messages.Accept_Encoding_Token);
      Output (Messages.Accept_Token);
      Output (Messages.Host_Token);
      Output (Messages.Range_Token);
      Output (Messages.If_Modified_Since_Token);
      Text_IO.New_Line;
      return Response.Build (MIME.Text_HTML, "ok");
   end CB;

   ----------
   -- Dump --
   ----------

   procedure Dump
     (Direction : in Net.Log.Data_Direction;
      Socket    : in Net.Socket_Type'Class;
      Data      : in Stream_Element_Array;
      Last      : in Stream_Element_Offset)
   is
      use type Net.Log.Data_Direction;
   begin
      if Direction = Net.Log.Sent then
         Text_IO.Put_Line
           ("********** " & Net.Log.Data_Direction'Image (Direction));
         Text_IO.Put_Line
           (Translator.To_String (Data (Data'First .. Last)));
         Text_IO.New_Line;
      end if;
   end Dump;

   R  : Response.Data;
   H  : Headers.List;

begin
   Get_Free_Port (Port);

   Server.Start
     (WS, "Client Headers", CB'Unrestricted_Access, Port => Port);

   --  AWS.Net.Log.Start (Dump'Unrestricted_Access);

   R := AWS.Client.Get
     (URL => "http://localhost:" & Utils.Image (Port) & "/1");
   R := AWS.Client.Head
     (URL => "http://localhost:" & Utils.Image (Port) & "/2");
   R := AWS.Client.Post
     (URL => "http://localhost:" & Utils.Image (Port) & "/3", Data => "V=1");

   Headers.Set.Add (H, Messages.Accept_Language_Token, "fr");
   Headers.Set.Add (H, Messages.If_Modified_Since_Token, "yesterday :)");

   R := AWS.Client.Get
     (URL => "http://localhost:" & Utils.Image (Port) & "/4", Headers => H);

   Headers.Set.Add (H, Messages.User_Agent_Token, "Very_Secret");

   R := AWS.Client.Head
     (URL => "http://localhost:" & Utils.Image (Port) & "/5", Headers => H);

   Headers.Set.Update (H, Messages.User_Agent_Token, "Or_Not");
   Headers.Set.Add (H, Messages.Host_Token, "me");

   R := AWS.Client.Post
     (URL => "http://localhost:" & Utils.Image (Port) & "/6",
      Data => "V=2", Headers => H);

   Server.Shutdown (WS);
   Text_IO.Put_Line ("shutdown");
end Client_Headers;
