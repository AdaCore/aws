------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2007-2012, AdaCore                     --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
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
     (Credential : Plain.Credential;
      Sock       : in out Net.Socket_Type'Class;
      Status     : out SMTP.Status)
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

   overriding function Image (Info : Credential) return String is
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
      return Translator.Base64_Encode
        (Translator.To_Stream_Element_Array (Message));
   end Image;

   ----------------
   -- Initialize --
   ----------------

   function Initialize (Auth_Cid, Password : String) return Credential is
      use Ada.Strings;

      Result : Credential;
      --  The Strings will be truncated as necessary.  Authentication
      --  will likely fail when the length of a credential exceeds a
      --  buffer size.

   begin
      pragma Assert
        (Auth_Cid'Length <= Result.Auth_Cid'Length
         and then Password'Length <= Result.Password'Length);

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
