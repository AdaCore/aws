------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2016, CNRS                         --
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

with AWS.Client;
with AWS.Response;

procedure REST_Client is
   Base_URI : constant String := "http://localhost:8080";
begin
   declare
      Answer : constant AWS.Response.Data :=
        AWS.Client.Get (Base_URI & "/parameters");
   begin
      Ada.Text_IO.Put_Line (AWS.Response.Message_Body (Answer));
   end;
   declare
      Answer : constant AWS.Response.Data :=
        AWS.Client.Get (Base_URI & "/parameters/name");
   begin
      Ada.Text_IO.Put_Line (AWS.Response.Message_Body (Answer));
   end;
   declare
      Answer : constant AWS.Response.Data :=
        AWS.Client.Post (Base_URI & "/parameters/new_parameter",
                         Data => "new_value");
   begin
      Ada.Text_IO.Put_Line (AWS.Response.Message_Body (Answer));
   end;
   declare
      Answer : constant AWS.Response.Data :=
        AWS.Client.Put (Base_URI & "/parameters/new_parameter",
                        Data => "new_value_2");
   begin
      Ada.Text_IO.Put_Line (AWS.Response.Message_Body (Answer));
   end;
   declare
      Empty_Stream : constant Ada.Streams.Stream_Element_Array (1 .. 0) :=
        (others => 0);
      Answer : constant AWS.Response.Data :=
        AWS.Client.Delete (Base_URI & "/parameters/host",
                           Data => Empty_Stream);
   begin
      Ada.Text_IO.Put_Line (AWS.Response.Message_Body (Answer));
   end;
end REST_Client;
