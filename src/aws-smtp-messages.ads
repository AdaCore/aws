------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                 S M T P - Simple Mail Transfer Protocol                  --
--                                                                          --
--                     Copyright (C) 2008-2009, AdaCore                     --
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

with AWS.Headers;
with AWS.Messages;

package AWS.SMTP.Messages is

   --  SMTP headers

   subtype Header_Name is String;

   From_Token          : constant Header_Name := "From";
   To_Token            : constant Header_Name := "To";
   Message_Id_Token    : constant Header_Name := "Message-ID";
   Subject_Token       : constant Header_Name := "Subject";
   MIME_Version_Token  : constant Header_Name := "MIME-Version";
   Content_Type_Token  : constant Header_Name :=
                           AWS.Messages.Content_Type_Token;
   Date_Token          : constant Header_Name :=
                           AWS.Messages.Date_Token;

   --  A message as reported by the server

   type Data is private;

   function Message_Body (Message : Data) return String;
   --  Returns the message body

   function Headers (Message : Data) return Headers.List;
   --  Returns the SMTP headers

private

   type Data is record
      Message_Body : Unbounded_String;
      Headers      : AWS.Headers.List;
   end record;

end AWS.SMTP.Messages;
