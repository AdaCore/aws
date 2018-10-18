------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2018, AdaCore                        --
--                                                                          --
--  This is free software;  you can redistribute it  and/or modify it       --
--  under terms of the  GNU General Public License as published  by the     --
--  Free Software  Foundation;  either version 3,  or (at your option) any  --
--  later version.  This software is distributed in the hope  that it will  --
--  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU     --
--  General Public License for  more details.                               --
--                                                                          --
--  You should have  received  a copy of the GNU General  Public  License   --
--  distributed  with  this  software;   see  file COPYING3.  If not, go    --
--  to http://www.gnu.org/licenses for a complete copy of the license.      --
------------------------------------------------------------------------------

with Ada.Streams;
with Ada.Text_IO; use Ada.Text_IO;

with AWS.Client;
with AWS.Config.Set;
with AWS.Default;
with AWS.Messages;
with AWS.Net.Log;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;
with AWS.Utils;

procedure Hide_Id is

   use Ada;
   use AWS;
   use type Streams.Stream_Element_Offset;

   WS     : Server.HTTP;
   CFG    : Config.Object := Config.Get_Current;
   Result : Response.Data;

   DS     : File_Type;

   Hello : String   := "/876543210";
   Last  : Positive;

   Adjust : constant Streams.Stream_Element_Offset := AWS.Version'Length - 4;

   Default_User_Agent : constant String :=
                          Messages.User_Agent (Default.User_Agent);

   --------------
   -- HTTP_Log --
   --------------

   procedure HTTP_Log
     (Direction : Net.Log.Data_Direction;
      Socket    : Net.Socket_Type'Class;
      Data      : Streams.Stream_Element_Array;
      Last      : Streams.Stream_Element_Offset)
   is
      procedure Write (F : File_Type);
      --  Write info into file F

      -----------
      -- Write --
      -----------

      procedure Write (F : File_Type) is
         Buffer : String (1 .. 1024);
         K      : Natural := 0;
         Output : Boolean := False;
         S1, S2 : Streams.Stream_Element_Offset;
      begin
         New_Line (F, 2);

         --  Adjust size depending on the AWS version length

         S1 := Last;
         S2 := Data'Last;

         case Direction is
            when Net.Log.Sent    =>
               S1 := S1 - Adjust;
               S2 := S2 - Adjust;
            when Net.Log.Received =>
               S1 := S1 - Adjust;
         end case;

         Put_Line (F, "@@@ " & Net.Log.Data_Direction'Image (Direction)
                   & " ("
                   & Streams.Stream_Element_Offset'Image (S1) & "/"
                   & Streams.Stream_Element_Offset'Image (S2)
                   & " buffer usage) @@@");

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
         when Net.Log.Received => null;
      end case;
   end HTTP_Log;

   -----------
   -- HW_CB --
   -----------

   function HW_CB (Request : Status.Data) return AWS.Response.Data is
      URI : constant String := AWS.Status.URI (Request);
   begin
      if URI = Hello (1 .. Last) then
         delay 0.25; -- wait a bit to have more chance to get a sync log
         return AWS.Response.Build ("text/html", "<p>Hello world !");
      else
         return AWS.Response.Build ("text/html", "<p>Hum...");
      end if;
   end HW_CB;

   ------------
   -- Output --
   ------------

   procedure Output (F : File_Type) is
      Buffer : String (1 .. 1024);
      Last   : Natural;
   begin
      while not End_Of_File (F) loop
         Get_Line (F, Buffer, Last);

         if Last > 1 and then Buffer (Last) = ASCII.CR then
            Last := Last - 1;
         end if;

         if Buffer (1 .. Last) = Default_User_Agent then
            Buffer (Last - 3  .. Last) := "##.#";
         end if;

         Put_Line (Buffer (1 .. Last));
      end loop;
   end Output;

begin
   Net.Log.Start (Write => HTTP_Log'Unrestricted_Access);

   Config.Set.Server_Name (CFG, "Hello World");
   Config.Set.Server_Port (CFG, 0);
   Config.Set.Protocol_Family (CFG, "Family_Inet");

   Server.Start (WS, HW_CB'Unrestricted_Access, CFG);

   if not Server.Status.Is_Any_Address (WS) then
      Put_Line ("AWS.Net.Std.Is_Any_Address error");
   end if;

   Create (DS, Out_File, "net_log_sent");

   --  Compensate port length in URL

   Last := Hello'Last - Utils.Image (Server.Status.Port (WS))'Length;

   Put_Line (DS, "=============== First");
   Result := Client.Get (Server.Status.Local_URL (WS) & Hello (1 .. Last));

   Put_Line (DS, "=============== Second");
   Result := Client.Get
     (Server.Status.Local_URL (WS) & Hello (1 .. Last),
      User_Agent => Config.User_Agent);

   Put_Line (DS, "=============== Third");
   Result := Client.Get
     (Server.Status.Local_URL (WS) & Hello (1 .. Last),
      User_Agent => "");

   Server.Shutdown (WS);
   Net.Log.Stop;

   Reset (DS, In_File);
   Output (DS);

   Delete (DS);
end Hide_Id;
