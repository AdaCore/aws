------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2017, AdaCore                     --
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

pragma Ada_2012;

--  This library implement the Simple Mail Transfer Protocol. Only part of the
--  RFC 821 is covered. There is no support to send a message to a console for
--  example.

with AWS.Net;

private with Ada.Strings.Unbounded;
limited with AWS.SMTP.Authentication;

package AWS.SMTP is

   Server_Error : exception;
   --  Raised when an unrecoverable error is found

   Reply_Code_Error : exception;
   --  Raised when a reply code error is not known

   Default_SMTP_Port : constant := 25;

   --------------
   -- Receiver --
   --------------

   type Receiver is private;
   --  The receiver part (i.e. a server) of SMTP messages as defined in
   --  RFC 821. This is the SMTP server.

   function Initialize
     (Server_Name : String;
      Port        : Positive := Default_SMTP_Port;
      Secure      : Boolean := False;
      Family      : Net.Family_Type := Net.Family_Unspec;
      Credential  : access constant Authentication.Credential'Class := null;
      Timeout     : Duration := Net.Forever)
      return Receiver;
   --  Create a Server composed of the Name and the Port (default SMTP port
   --  is 25), this server will be used to send SMTP message.

   ----------------
   -- Reply_Code --
   ----------------

   type Reply_Code is range 200 .. 554;

   Service_Ready       : constant Reply_Code := 220;
   Service_Closing     : constant Reply_Code := 221;
   Auth_Successful     : constant Reply_Code := 235;
   Requested_Action_Ok : constant Reply_Code := 250;
   Provide_Watchword   : constant Reply_Code := 334;
   Start_Mail_Input    : constant Reply_Code := 354;
   Syntax_Error        : constant Reply_Code := 500;

   function Image (R : Reply_Code) return String;
   --  Returns the reply code as a string. Raises Reply_Code_Error if R is
   --  not a valid reply code.

   function Name (R : Reply_Code) return String;
   --  Returns the reply code reason string. Raises Reply_Code_Error if R is
   --  not a valid reply code.

   function Message (R : Reply_Code) return String;
   --  This returns the value: Image (R) & ' ' & Name (R)

   ------------
   -- Status --
   ------------

   type Status is private;

   function Is_Ok (Status : SMTP.Status) return Boolean with Inline;
   --  Return True is status if Ok (no problem) or false if a problem has been
   --  detected. This is not an error (in that case Error is raised) but a
   --  warning because something wrong (but not unrecoverable) has happen.

   function Status_Message (Status : SMTP.Status) return String;
   --  If Is_Ok is False, this function return the reason of the problem. The
   --  return message is the error message as reported by the server.

   function Warnings (Status : SMTP.Status) return String with Inline;
   --  Returns warnings during recipient addresses processing

   function Status_Code (Status : SMTP.Status) return Reply_Code with Inline;
   --  Returns the code replied by the server

   procedure Clear (Status : in out SMTP.Status) with Inline;
   --  Clear Status value. Code is set to Requested_Action_Ok and message
   --  string to null.

   -----------------
   -- E_Mail_Data --
   -----------------

   type E_Mail_Data is private;

   type Address_Mode is (Full, Name, Address);

   function Image
     (E_Mail : E_Mail_Data;
      Mode   : Address_Mode := Full) return String;
   --  Returns E_Mail only (Mode = Address), recipient name only (Mode = Name)
   --  or Name and e-mail (Mode = Full).

   function E_Mail (Name : String; Address : String) return E_Mail_Data;
   --  Returns an e-mail address

   function Parse (E_Mail : String) return E_Mail_Data;
   --  Parse an e-mail with format "Name <address>" or "address (Name)"
   --  and Returns the corresponding E_Mail_Data. Raises Contraint_Error
   --  if E_Mail can't be parsed.

   type Recipients is array (Positive range <>) of E_Mail_Data;

   No_Recipient : constant Recipients;

private

   use Ada.Strings.Unbounded;

   type Receiver is record
      Family  : Net.Family_Type;
      Name    : Unbounded_String;
      Port    : Positive;
      Secure  : Boolean;
      Sock    : Net.Socket_Access;
      Auth    : access constant Authentication.Credential'Class;
      Timeout : Duration;
   end record;

   type Status is record
      Code     : Reply_Code;
      Reason   : Unbounded_String;
      Warnings : Unbounded_String;
   end record;

   type E_Mail_Data is record
      Name    : Unbounded_String;
      Address : Unbounded_String;
   end record;

   No_Recipient : constant Recipients (1 .. 0) := (others => <>);

   --  Server Reply code/reason

   type Server_Reply is new Status;

   function Image (Answer : Server_Reply) return String;
   --  Returns the string representation for Answer

   procedure Add (Answer : in out Server_Reply; Status : in out SMTP.Status);
   --  Add status code and reason to the list of server's reply

   procedure Check_Answer
     (Sock  : Net.Socket_Type'Class;
      Reply : out Server_Reply);
   --  Read a reply from the SMTP server (listening on Sock) and fill the Reply
   --  structure.

end AWS.SMTP;
