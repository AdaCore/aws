------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                   S M T P - Simple Mail Transfer Protocol                --
--                                                                          --
--                           Copyright (C) 2007                             --
--                                 AdaCore                                  --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

with Ada.Strings.Fixed;

with AWS.Net.Buffered;
with AWS.Translator;

package body AWS.SMTP.Authentication.Plain is

   use Ada;

   -----------------
   -- Before_Send --
   -----------------

   overriding procedure Before_Send
     (Credential : in     Plain.Credential;
      Sock       : in out Net.Socket_Type'Class;
      Status     :    out SMTP.Status)
   is
      Answer : Server_Reply;
   begin
      Net.Buffered.Put_Line (Sock, "AUTH PLAIN " & Image (Credential));

      Check_Answer (Sock, Answer);

      --  here we might get a request code to provide authentication data, or
      --  the usual Requested_Action_Ok. (If the credentials image in
      --  Server.Auth has been accepted along with the AUTH PLAIN <image>).

      if Answer.Code = Auth_Successful then
         null;

      elsif Answer.Code = Requested_Action_Ok then
         null;

      elsif Answer.Code = Provide_Watchword then
         Net.Buffered.Put_Line (Sock, Image (Credential));

         Check_Answer (Sock, Answer);

         if Answer.Code /= Auth_Successful then
            Add (Answer, Status);
         end if;

      else
         Add (Answer, Status);
      end if;
   end Before_Send;

   -----------
   -- Image --
   -----------

   overriding function Image (Info : in Credential) return String is
      UTF_8_NUL : constant Character := Character'Val (0);
      --  Part of AUTH PLAIN message text, acting as a separator

      --  The message to be sent consists of parts as desribed in
      --  RFC4616, 2. PLAIN SASL mechanism,
      --
      --   message   = [authzid] UTF8NUL authcid UTF8NUL passwd
      --
      --  Authzid is not used.

      Message : constant String :=
                  UTF_8_NUL & Info.Auth_Cid (1 .. Info.Last_A)
                  & UTF_8_NUL & Info.Password (1 .. Info.Last_P);

      --  The Base64 Encode function expects a Stream_Element_Array.
      --  Assume that characters can safely be interpreted as stream
      --  elements and therefore use unchecked conversion.

   begin
      pragma Assert
        (Info.Last_A <= Info.Auth_Cid'Last
         and Info.Last_P <= Info.Password'Last);

      return Translator.Base64_Encode
        (Translator.To_Stream_Element_Array (Message));
   end Image;

   ----------------
   -- Initialize --
   ----------------

   function Initialize (Auth_Cid, Password : in String) return Credential is
      use Ada.Strings;

      Result : Credential;
      --  The Strings will be truncated as necessary.  Authentication
      --  will likely fail when the length of a credential exceeds a
      --  buffer size.

   begin
      pragma Assert
        (Auth_Cid'Length <= Result.Auth_Cid'Length
         and Password'Length <= Result.Password'Length);

      Fixed.Move
        (Source  => Auth_Cid,
         Target  => Result.Auth_Cid,
         Drop    => Right,
         Justify => Left,
         Pad     => '#');

      Result.Last_A := Positive'Min (Result.Auth_Cid'Length, Auth_Cid'Length);

      Fixed.Move
        (Source  => Password,
         Target  => Result.Password,
         Drop    => Right,
         Justify => Left,
         Pad     => '#');

      Result.Last_P := Positive'Min (Result.Password'Length, Password'Length);

      return Result;
   end Initialize;

end AWS.SMTP.Authentication.Plain;
