------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                   S M T P - Simple Mail Transfer Protocol                --
--                                                                          --
--                         Copyright (C) 2000-2002                          --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimkov - Pascal Obry                                --
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

--  $Id$

--  This library implement the Simple Mail Transfer Protocol. Only part of the
--  RFC 821 is covered. There is no support to send a message to a console for
--  example.

with Ada.Strings.Unbounded;

with AWS.Net;

package AWS.SMTP is

   Server_Error : exception;
   --  Raised when an unrecoverable error is found.

   Reply_Code_Error : exception;
   --  Raised when a reply code error is not known.

   Default_SMTP_Port : constant := 25;

   type Reply_Code is range 200 .. 554;

   Service_Ready       : constant Reply_Code := 220;
   Service_Closing     : constant Reply_Code := 221;
   Requested_Action_Ok : constant Reply_Code := 250;
   Start_Mail_Input    : constant Reply_Code := 354;
   Syntax_Error        : constant Reply_Code := 500;

   function Image (R : in Reply_Code) return String;
   --  Returns the reply code as a string. Raises Reply_Code_Error if R is
   --  not a valid reply code.

   function Name (R : in Reply_Code) return String;
   --  Returns the reply code reason string. Raises Reply_Code_Error if R is
   --  not a valid reply code.

   function Message (R : in Reply_Code) return String;
   --  This returns the value: Image (R) & ' ' & Name (R)

   type Receiver is private;
   --  The receiver part (i.e. a server) of SMTP messages as defined in
   --  RFC 821. This is the SMTP server

   type Status is private;

   function Is_Ok (Status : in SMTP.Status) return Boolean;
   pragma Inline (Is_Ok);
   --  Return True is status if Ok (no problem) or false if a problem has been
   --  detected. This is not an error (in that case Error is raised) but a
   --  warning because something wrong (but not unrecoverable) has happen.

   function Status_Message (Status : in SMTP.Status) return String;
   --  If Is_Ok is False, this function return the reason of the problem. The
   --  return message is the error message as reported by the server.

   function Status_Code (Status : in SMTP.Status) return Reply_Code;
   pragma Inline (Status_Code);
   --  Returns the code replied by the server.

   procedure Clear (Status : in out SMTP.Status);
   pragma Inline (Clear);
   --  Clear Status value. Code is set to Requested_Action_Ok and message
   --  string to null.

   type E_Mail_Data is private;

   function E_Mail (Name : in String; Address : in String)
     return E_Mail_Data;
   --  Returns an e-mail address with the format "Name <address>"

   type Recipients is array (Positive range <>) of E_Mail_Data;

private

   use Ada.Strings.Unbounded;

   type Receiver is record
      Name : Unbounded_String;
      Port : Positive;
      Sock : Net.Socket_Access;
   end record;

   type Status is record
      Value : Unbounded_String;
      Code  : Reply_Code;
   end record;

   type E_Mail_Data is record
      Name    : Unbounded_String;
      Address : Unbounded_String;
   end record;

end AWS.SMTP;
