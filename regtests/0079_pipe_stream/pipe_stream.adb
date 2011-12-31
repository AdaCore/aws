------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2007-2012, AdaCore                     --
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

with Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Ada.Text_IO.Text_Streams;

with GNAT.OS_Lib;

with AWS.Client;
with AWS.MIME;
with AWS.Server;
with AWS.Status;
with AWS.Resources.Streams.Pipe;
with AWS.Response;
with AWS.Utils;

with Get_Free_Port;

procedure Pipe_Stream is

   use Ada;
   use GNAT;
   use AWS;

   WS : Server.HTTP;

   Stdout : constant Text_IO.Text_Streams.Stream_Access :=
              Text_IO.Text_Streams.Stream (Text_IO.Current_Output);
   --  Used for raw output, avoid any translations

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      Args : OS_Lib.Argument_List (1 .. 1) := (1 => new String'("-pipe"));
      Strm : Resources.Streams.Stream_Access;
   begin
      Strm :=  new Resources.Streams.Pipe.Stream_Type;

      Resources.Streams.Pipe.Open
        (Resources.Streams.Pipe.Stream_Type (Strm.all), "./pipe_stream", Args);

      return Response.Stream (MIME.Application_Octet_Stream, Strm);
   end CB;

   --------------
   -- Run_Test --
   --------------

   procedure Run_Test is
      Port : Natural := 8456;
      R    : Response.Data;
   begin
      Get_Free_Port (Port);

      Server.Start (WS, "pipe", CB'Unrestricted_Access, Port => Port);

      R := Client.Get ("http://localhost:" & Utils.Image (Port));

      declare
         M : constant String := Response.Message_Body (R);
         I : Natural := M'First - 1;
      begin
         Text_IO.Put_Line ("Message length " & M'Length'Img);

         Main : for K in 1 .. 10 loop
            for C in 0 .. 255 loop
               I := I + 1;
               exit when I > M'Last or else M (I) /= Character'Val (C);
            end loop;
         end loop Main;

         if I = 2560 then
            Text_IO.Put_Line ("OK");
         else
            Text_IO.Put_Line ("NOK on " & Utils.Image (I));
         end if;
      end;

      Server.Shutdown (WS);
   end Run_Test;

   --------------
   -- Pipe_Out --
   --------------

   procedure Pipe_Out is
   begin
      for K in 1 .. 10 loop
         for C in 0 .. 255 loop
            Character'Write (Stdout, Character'Val (C));
         end loop;
         delay 1.0 / Duration (K);
      end loop;
   end Pipe_Out;

begin
   if Command_Line.Argument_Count = 0 then
      Run_Test;
   else
      Pipe_Out;
   end if;
end Pipe_Stream;
