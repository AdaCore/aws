------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2007-2016, AdaCore                     --
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

pragma Ada_2012;

with Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;
with Ada.Synchronous_Barriers;
with Ada.Text_IO.Text_Streams;

with GNAT.OS_Lib;

with AWS.Client;
with AWS.MIME;
with AWS.Resources.Streams.Pipe;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;
with AWS.Utils;

procedure Pipe_Stream is

   use Ada;
   use AWS;
   use GNAT;
   use Ada.Strings.Unbounded;

   package ASB renames Ada.Synchronous_Barriers;

   WS : Server.HTTP;
   Barrier : ASB.Synchronous_Barrier (8);

   Stdout : constant Text_IO.Text_Streams.Stream_Access :=
              Text_IO.Text_Streams.Stream (Text_IO.Current_Output);
   --  Used for raw output, avoid any translations

   procedure Run_Test
     (Output : out Unbounded_String; Option : String := "-pipe");

   task type Tester is
      entry Result (Output : out Unbounded_String);
   end Tester;

   procedure On_Error (Status : Integer; Error : String);

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      Args : OS_Lib.Argument_List (1 .. 1) :=
        (1 => new String'(Status.Parameter (Request, "option")));
      Strm : Resources.Streams.Stream_Access;
   begin
      Strm := new Resources.Streams.Pipe.Stream_Type;

      Resources.Streams.Pipe.Open
        (Resources.Streams.Pipe.Stream_Type (Strm.all), "./pipe_stream", Args,
         On_Error => On_Error'Unrestricted_Access);

      return Response.Stream (MIME.Application_Octet_Stream, Strm);
   end CB;

   --------------
   -- On_Error --
   --------------

   procedure On_Error (Status : Integer; Error : String) is
   begin
      Text_IO.Put_Line ("Error code:" & Status'Img);
      Text_IO.Put_Line ("Error message: " & Error);
   end On_Error;

   --------------
   -- Run_Test --
   --------------

   procedure Run_Test
     (Output : out Unbounded_String; Option : String := "-pipe")
   is
      R : Response.Data;
   begin
      R := Client.Get (Server.Status.Local_URL (WS) & "?option=" & Option);

      declare
         M : constant String := Response.Message_Body (R);
         I : Natural := M'First - 1;
      begin
         Append (Output, "Message length " & M'Length'Img & ASCII.LF);

         Main : for K in 1 .. 100 loop
            for C in 0 .. 255 loop
               I := I + 1;
               exit when I > M'Last or else M (I) /= Character'Val (C);
            end loop;
         end loop Main;

         if I = 25600 then
            Append (Output, "OK" & ASCII.LF);
         else
            Append (Output, "NOK on " & Utils.Image (I) & ASCII.LF);
         end if;
      end;
   end Run_Test;

   ------------
   -- Tester --
   ------------

   task body Tester is
      O : Unbounded_String;
      Dummy : Boolean;
   begin
      ASB.Wait_For_Release (Barrier, Dummy);
      Run_Test (O);

      accept Result (Output : out Unbounded_String) do
         Output := O;
      end Result;
   end Tester;

   --------------
   -- Pipe_Out --
   --------------

   procedure Pipe_Out is
   begin
      for K in 1 .. 100 loop
         for C in 0 .. 255 loop
            Character'Write (Stdout, Character'Val (C));
         end loop;
         delay 1.0 / Duration (K);
      end loop;
   end Pipe_Out;

   Output : Unbounded_String;
   Prev   : Unbounded_String;

   TA : array (1 .. Barrier.Release_Threshold) of access Tester;
   Cnt : Natural := 0;

   ------------------
   -- Check_Output --
   ------------------

   procedure Check_Output is
   begin
      if Prev /= Output then
         Text_IO.Put (To_String (Output));
         Prev := Output;
      end if;
   end Check_Output;

begin
   if Command_Line.Argument_Count = 0 then
      Server.Start (WS, "pipe", CB'Unrestricted_Access, Port => 0);

      for T of TA loop
         T := new Tester;
      end loop;

      for T of TA loop
         select
            T.Result (Output);
            T := null;
            Check_Output;
         or delay 0.1;
            Cnt := Cnt + 1;
            Output := Null_Unbounded_String;
            Run_Test (Output);
            Check_Output;
         end select;
      end loop;

      for T of TA loop
         if T /= null then
            T.Result (Output);
            Check_Output;
            Cnt := Cnt - 1;
         end if;
      end loop;

      if Cnt /= 0 then
         Text_IO.Put_Line ("Counter error" & Cnt'Img);
      end if;

      Run_Test (Output, "Wrong_Parameter");

      Server.Shutdown (WS);

   elsif Command_Line.Argument (1) = "-pipe" then
      Pipe_Out;

   else
      Text_IO.Put_Line
        (Text_IO.Current_Error,
         "Wrong parameter " & Command_Line.Argument (1));
      Command_Line.Set_Exit_Status (123);
   end if;
end Pipe_Stream;
