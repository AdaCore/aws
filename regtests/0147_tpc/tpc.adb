------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2012, AdaCore                     --
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
with Ada.Text_IO;

with AWS.Services.Transient_Pages.Control;
with AWS.Resources.Streams.Memory;

procedure TPC is
   use AWS.Services.Transient_Pages;
   use AWS.Resources;

   Stream : Streams.Stream_Access;

   Buffer : Ada.Streams.Stream_Element_Array := (1 .. 200 => 31);
begin
   for J in 1 .. 1000 loop
      Stream := new AWS.Resources.Streams.Memory.Stream_Type;

      for K in 1 .. 10 loop
         Streams.Memory.Append
           (Streams.Memory.Stream_Type (Stream.all), Buffer);
      end loop;

      Register (Get_URI, Stream, 0.1);
   end loop;

   Control.Register (0.1);

   delay 0.5;

   Control.Shutdown;

   Ada.Text_IO.Put_Line ("Done.");
end TPC;
