------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2014-2016, AdaCore                     --
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

with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with AWS.Net.SSL;
with AWS.OS_Lib;
with AWS.Utils;

procedure ErrCon is

   use Ada;
   use Ada.Strings;
   use AWS;

   Sock : Net.Socket_Type'Class := Net.Socket (AWS.Net.SSL.Is_Supported);

   Port_Img : constant String := "10123";
   Port_Vol : constant Positive := Positive'Value (Port_Img);

   Refused_Err : constant String :=
                   '[' & Utils.Image (Natural (OS_Lib.ECONNREFUSED)) & ']';

   Unreach_Err : constant String :=
                   '[' & Utils.Image (Natural (OS_Lib.ENETUNREACH)) & ']';

   No_Ent_Err : constant String :=
                   '[' & Utils.Image (Natural (OS_Lib.ENOENT)) & ']';

   procedure Print_Error (Message : String);

   -----------------
   -- Print_Error --
   -----------------

   procedure Print_Error (Message : String) is
      Text : String := Message;
   begin
      if Text (Text'Last - 10 .. Text'Last) = "[::1]:" & Port_Img then
         Print_Error
           (Text (Text'First .. Text'Last - 11) & "127.0.0.1:" & Port_Img);

      elsif Utils.Match (Text, Refused_Err) then
         Text_IO.Put_Line
           (Fixed.Replace_Slice
              (Text, Text'First, Refused_Err'Length, "[REFUSED]"));

      elsif Utils.Match (Text, Unreach_Err) then
         Text_IO.Put_Line
           (Fixed.Replace_Slice
              (Text, Text'First, Unreach_Err'Length, "[UNREACH]"));

      elsif Utils.Match (Text, No_Ent_Err) then
         Text_IO.Put_Line
           (Fixed.Replace_Slice
              (Text, Text'First, No_Ent_Err'Length, "[NO_NAME]"));

      else
         Text_IO.Put_Line (Text);
      end if;
   end Print_Error;

   Hosts : array (1 .. 4) of access String :=
            (new String'("localhost"),
             new String'("127.0.0.1"),
             new String'("255.255.255.255"),
             new String'("no-way.to-have.such-address"));

begin
   for Host of Hosts loop
      begin
         Sock.Connect (Host => Host.all, Port => Port_Vol);
      exception
         when E : AWS.Net.Socket_Error =>
            Print_Error (Ada.Exceptions.Exception_Message (E));
      end;
   end loop;
end ErrCon;
