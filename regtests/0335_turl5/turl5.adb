------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;
with AWS.URL;        use AWS.URL;

procedure Turl5 is

   procedure Test (URL : String) is
      O : Object;
   begin
      O := Parse (URL);

      Put_Line (URL);
      Put_Line (AWS.URL.URL (O));
      Put_Line ("   Server     : " & Host (O));
      Put_Line ("   Port       : " & Port (O));
      Put_Line ("   Path       : " & Path (O));
      Put_Line ("   File       : " & File (O));
      Put_Line ("   Parameters : " & Parameters (O));
      Put_Line ("   URI        : " & Pathname_And_Parameters (O));

      Normalize (O);

      Put_Line ("   *");
      Put_Line ("   Server     : " & Host (O));
      Put_Line ("   Port       : " & Port (O));
      Put_Line ("   Path       : " & Path (O));
      Put_Line ("   File       : " & File (O));
      Put_Line ("   Parameters : " & Parameters (O));
      Put_Line ("   URI        : " & Pathname_And_Parameters (O));

   exception
      when E : URL_Error =>
         Put_Line ("Error on " & URL & " - " & Exception_Message (E));
   end Test;

   URL_Host : constant String :=
                "http://ftp.ch.debian.org";
   URL_Path : constant String :=
                "/debian/pool/main/g/git/git_2.20.1-2+deb10u3_amd64.deb";

begin
   Test (URL_Host & URL_Path);
end Turl5;
