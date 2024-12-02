------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2024, AdaCore                        --
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

with Ada.Text_IO;
with Ada.Exceptions;

with AWS.Client;
with AWS.Response;
with AWS.Net.SSL.Certificate;

procedure Check_Cert is

   use Ada;
   use Ada.Exceptions;

   function Clean_Up_Response (Content : String) return String;
   --  Only first line, remove traceback

   procedure Check (URL : String);
   --  Check a bad URL and report error

   ---------------
   -- Check_Def --
   ---------------

   procedure Check_Def (URL : String) is
      use AWS;

      Conn : Client.HTTP_Connection;
      Data : Response.Data;
   begin
      Text_IO.Put_Line ("CHECK: default SSL setup");

      --  Issue a Get on a connection established with the default SSL
      --  setting (request to check certificates which is the default).

      Client.Create
        (Connection => Conn,
         Host       => URL);

      Client.Get (Conn, Data);

      Text_IO.Put_Line ("  Status Code: " & Response.Status_Code (Data)'Img);
      Text_IO.Put_Line
        ("  Response: "
         & Clean_Up_Response (Response.Message_Body (Data)));
   exception
      when E : others =>
         Text_IO.Put_Line (Exception_Message (E));
   end Check_Def;

   ---------------
   -- Check_Cnf --
   ---------------

   procedure Check_Cnf (URL : String; Check_Certificate : Boolean) is
      use AWS;

      Conn    : Client.HTTP_Connection;
      Data    : Response.Data;
      SSL_Cfg : AWS.Net.SSL.Config;

   begin
      Text_IO.Put_Line
        ("CHECK: SSL config with Check_Certificate = "
         & Check_Certificate'Image);

      AWS.Net.SSL.Initialize
        (Config               => SSL_Cfg,
         Security_Mode        => AWS.Net.SSL.TLS_Client,
         Check_Certificate    => Check_Certificate,
         Exchange_Certificate => True);

      --  Issue a Get on a connection established with an explicit
      --  request to check or ignore certificates.
      --
      --  When checking certificate we expect a S4xx response
      --  otherwise S200 status code is expected.

      Client.Create
        (Connection => Conn,
         Host       => URL,
         SSL_Config => SSL_Cfg);

      Client.Get (Conn, Data);

      Text_IO.Put_Line ("  Status Code: " & Response.Status_Code (Data)'Img);

      --  Don't display the actual body if we do not check certificate, this
      --  has no meaning as a standard Web page is returned.

      if Check_Certificate then
         Text_IO.Put_Line
           ("  Response: "
            & Clean_Up_Response (Response.Message_Body (Data)));
      end if;
   exception
      when E : others =>
         Text_IO.Put_Line (Exception_Message (E));
   end Check_Cnf;

   -----------
   -- Check --
   -----------

   procedure Check (URL : String) is
   begin
      --  We check three cases
      Text_IO.Put_Line ("-------------------------------------------------");
      Text_IO.Put_Line ("URL : " & URL);

      --  Check that the default SSL configuration is properly initialized
      Check_Def (URL);
      --  Check that when using a specific configuration the check is done
      Check_Cnf (URL, Check_Certificate => True);
      --  Check that when using a specific configuration without certificate
      --  checking we don't fail.
      Check_Cnf (URL, Check_Certificate => False);
      Text_IO.New_Line;
   end Check;

   -----------------------
   -- Clean_Up_Response --
   -----------------------

   function Clean_Up_Response (Content : String) return String is
      C    : String := Content;
      Last : Natural := C'First;
   begin
      while Last in C'Range
        and then C (Last) /= ASCII.CR
        and then C (Last) /= ASCII.LF
      loop
         if C (Last) in '0' .. '9' then
            C (Last) := 'x';
         end if;
         Last := Last + 1;
      end loop;

      return C (C'First .. Last);
   end Clean_Up_Response;

begin
   Check ("https://expired.badssl.com");
   Check ("https://wrong.host.badssl.com");
   Check ("https://self-signed.badssl.com");
   Check ("https://untrusted-root.badssl.com");
end Check_Cert;
