------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2004                            --
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

--  ~ MAIN [STD]

with Ada.Streams;
with Ada.Text_IO; use Ada.Text_IO;

with AWS.Client;
with AWS.Net.Log;
with AWS.Response;
with AWS.Server;
with AWS.Status;
with AWS.Utils;

with Get_Free_Port;

procedure Test_Net_Log is

   use Ada;
   use AWS;

   WS     : AWS.Server.HTTP;
   Port   : Natural := 4789;
   Result : Response.Data;

   DS, DR : File_Type;

   --------------
   -- HTTP_Log --
   --------------

   procedure HTTP_Log
     (Direction : in Net.Log.Data_Direction;
      FD        : in Integer;
      Data      : in Streams.Stream_Element_Array;
      Last      : in Streams.Stream_Element_Offset)
   is
      procedure Write (F : in File_Type);
      --  Write info into file F

      -----------
      -- Write --
      -----------

      procedure Write (F : in File_Type) is
         Buffer : String (1 .. 1024);
         K      : Natural := 0;
         Output : Boolean := False;
      begin
         New_Line (F, 2);
         Put_Line (F, "@@@ " & Net.Log.Data_Direction'Image (Direction)
                   & " (" &
                   Streams.Stream_Element_Offset'Image (Last) & "/" &
                   Streams.Stream_Element_Offset'Image (Data'Last) &
                   " buffer usage) @@@");

         for I in Data'First .. Last loop
            K := K + 1;
            Buffer (K) := Character'Val (Data (I));

            if Buffer (K) = ASCII.CR or else Buffer (K) = ASCII.LF then
               Output := True;
            end if;

            if Output then
               Put (F, Buffer (K));
            end if;

            if Buffer (K) = ASCII.LF then
               K := 0;
            elsif Buffer (K) = ':' then
               if Buffer (1 .. K) = "Date:"
                 or else Buffer (1 .. K) = "Host:"
                 or else Buffer (1 .. K) = "User-Agent:"
                 or else Buffer (1 .. K) = "Server:"
               then
                  Output := False;
               end if;
            end if;
         end loop;

         New_Line (F);
      end Write;

   begin
      case Direction is
         when Net.Log.Sent     => Write (DS);
         when Net.Log.Received => Write (DR);
      end case;
   end HTTP_Log;

   -----------
   -- HW_CB --
   -----------

   function HW_CB
     (Request : in Status.Data)
      return AWS.Response.Data
   is
      URI : constant String := AWS.Status.URI (Request);
   begin
      if URI = "/hello" then
         delay 3.0; -- wait a bit to have more chance to get a sync log
         return AWS.Response.Build ("text/html", "<p>Hello world !");
      else
         return AWS.Response.Build ("text/html", "<p>Hum...");
      end if;
   end HW_CB;

   ------------
   -- Output --
   ------------

   procedure Output (F : in File_Type) is
      Buffer : String (1 .. 1024);
      Last   : Natural;
   begin
      while not End_Of_File (F) loop
         Get_Line (F, Buffer, Last);
         Put_Line (Buffer (1 .. Last));
      end loop;
   end Output;

begin
   Get_Free_Port (Port);

   Net.Log.Start (Write => HTTP_Log'Unrestricted_Access);

   Server.Start
     (WS, "Hello World", Callback => HW_CB'Unrestricted_Access, Port => Port);

   Create (DS, Out_File, "net_log_sent");
   Create (DR, Out_File, "net_log_received");

   Result := Client.Get ("http://localhost:" & Utils.Image (Port) & "/hello");

   Server.Shutdown (WS);
   Net.Log.Stop;

   Reset (DS, In_File);
   Output (DS);
   Reset (DR, In_File);
   Output (DR);

   Delete (DS);
   Delete (DR);
end Test_Net_Log;
