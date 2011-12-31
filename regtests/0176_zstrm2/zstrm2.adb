------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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

with AWS.Messages;
with AWS.Response.Set;
with AWS.Translator;

procedure Zstrm2 is

   use Ada;
   use AWS;

   Message : constant String := "a message";

   procedure Run (Encoding : Messages.Content_Encoding) is
      Answer : Response.Data;
   begin
      Response.Set.Data_Encoding (Answer, Encoding);

      Response.Set.Message_Body
        (Answer, Translator.To_Stream_Element_Array (Message));
   end Run;

begin
   Run (Messages.Identity);
   Run (Messages.Deflate);
   Run (Messages.Gzip);

   Ada.Text_IO.Put_Line ("Done.");
end Zstrm2;
