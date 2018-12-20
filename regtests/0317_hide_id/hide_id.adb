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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with AWS.Client;
with AWS.Config.Set;
with AWS.Default;
with AWS.Messages;
with AWS.Net.Log;
with AWS.Response.Set;
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

   Hello : constant String   := "/876543210";
   Last  : Positive;

   Request_Header : constant String := "X-Request";
   Answer_Header  : constant String := "X-Answer";
   Header : Client.Header_List;
   Answer_Data : Unbounded_String;

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
      F : Unbounded_String;
      R : Boolean := False; -- Request

      procedure Write;
      --  Write info into file F

      -----------
      -- Write --
      -----------

      procedure Write is
         Buffer : String (1 .. 1024);
         K      : Natural := 0;
         Output : Boolean := False;
         S1, S2 : Streams.Stream_Element_Offset;
      begin
         Append (F, ASCII.LF & ASCII.LF);

         S1 := Last;
         S2 := Data'Last;

         for I in Data'First .. Last loop
            K := K + 1;
            Buffer (K) := Character'Val (Data (I));

            if Buffer (K) = ASCII.CR or else Buffer (K) = ASCII.LF then
               if Buffer (1 .. K - 1) = Default_User_Agent then
                  --  Adjust size depending on the AWS version length

                  case Direction is
                     when Net.Log.Sent    =>
                        S1 := S1 - Adjust;
                        S2 := S2 - Adjust;
                     when Net.Log.Received =>
                        S1 := S1 - Adjust;
                  end case;
               end if;

               Output := True;
            end if;

            if Output then
               Append (F, Buffer (K));
            end if;

            if Buffer (K) = ASCII.LF then
               K := 0;

            elsif Buffer (K) = ':' then
               if Buffer (1 .. K) = "Date:"
                 or else Buffer (1 .. K) = "Host:"
               then
                  Output := False;

               elsif Buffer (1 .. K - 1) = Request_Header then
                  R := True;
               end if;
            end if;
         end loop;

         Append (F, ASCII.LF & "@@@ "
                 & Net.Log.Data_Direction'Image (Direction)
                 & " (" & Streams.Stream_Element_Offset'Image (S1) & "/"
                 & Streams.Stream_Element_Offset'Image (S2)
                 & " buffer usage) @@@");
      end Write;

   begin
      case Direction is
         when Net.Log.Sent     =>
            Write;

            if R then
               Put_Line (DS, To_String (F));
            else
               Answer_Data := F;
            end if;
         when Net.Log.Received => null;
      end case;

   end HTTP_Log;

   -----------
   -- HW_CB --
   -----------

   function HW_CB (Request : Status.Data) return AWS.Response.Data is
      URI : constant String := AWS.Status.URI (Request);
      Answer : AWS.Response.Data :=
                 AWS.Response.Build
                   ("text/html",
                    (if URI = Hello (1 .. Last)
                     then "<p>Hello world !"
                     else "<p>Hum..."));
   begin
      Response.Set.Add_Header
        (Answer, Answer_Header, Status.Header (Request).Get (Request_Header));
      return Answer;
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
            for J in reverse 1 .. Last loop
               if Buffer (J) = 'v' then
                  Last := J + 4;
                  Buffer (J + 1 .. Last) := "##.#";
                  exit;
               end if;
            end loop;
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
   Answer_Data := Null_Unbounded_String;
   Header.Update (Request_Header, "1st-First");
   Result := Client.Get
     (Server.Status.Local_URL (WS) & Hello (1 .. Last), Headers => Header);
   while Answer_Data = Null_Unbounded_String loop
      delay Duration'Small;
   end loop;
   Put_Line (DS, To_String (Answer_Data));

   Put_Line (DS, "=============== Second");
   Answer_Data := Null_Unbounded_String;
   Header.Update (Request_Header, "2nd-Second");
   Result := Client.Get
     (Server.Status.Local_URL (WS) & Hello (1 .. Last),
      User_Agent => Config.User_Agent, Headers => Header);
   while Answer_Data = Null_Unbounded_String loop
      delay Duration'Small;
   end loop;
   Put_Line (DS, To_String (Answer_Data));

   Put_Line (DS, "=============== Third");
   Answer_Data := Null_Unbounded_String;
   Header.Update (Request_Header, "3rd-Third");
   Result := Client.Get
     (Server.Status.Local_URL (WS) & Hello (1 .. Last),
      User_Agent => "", Headers => Header);
   while Answer_Data = Null_Unbounded_String loop
      delay Duration'Small;
   end loop;
   Put_Line (DS, To_String (Answer_Data));

   Server.Shutdown (WS);
   Net.Log.Stop;

   Reset (DS, In_File);
   Output (DS);

   Delete (DS);
end Hide_Id;
