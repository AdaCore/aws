------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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

with Ada.Finalization;
with Ada.Strings.Unbounded;

with AWS.Headers;
with AWS.Net.Std;
with AWS.Resources.Streams;
with AWS.Utils;

package AWS.POP is

   use Ada.Strings.Unbounded;

   POP_Error : exception;
   --  Raised by all routines when an error has been detected

   -------------
   -- Mailbox --
   -------------

   Default_POP_Port : constant := 110;

   type Mailbox is private;

   type Authenticate_Mode is (Clear_Text, APOP);

   function Initialize
     (Server_Name  : String;
      User         : String;
      Password     : String;
      Authenticate : Authenticate_Mode := Clear_Text;
      Port         : Positive          := Default_POP_Port) return Mailbox;
   --  Connect on the given Port to Server_Name and open User's Mailbox. This
   --  mailbox object will be used to retrieve messages.

   procedure Close (Mailbox : POP.Mailbox);
   --  Close mailbox

   function User_Name (Mailbox : POP.Mailbox) return String;
   --  Returns User's name for this mailbox

   function Message_Count (Mailbox : POP.Mailbox) return Natural;
   --  Returns the number of messages in the user's mailbox

   function Size (Mailbox : POP.Mailbox) return Natural;
   --  Returns the total size in bytes of the user's mailbox

   -------------
   -- Message --
   -------------

   type Message is tagged private;

   function Get
     (Mailbox : POP.Mailbox;
      N       : Positive;
      Remove  : Boolean     := False) return Message;
   --  Retrieve Nth message from the mailbox, let the message on the mailbox
   --  if Remove is False.

   procedure Delete
     (Mailbox : POP.Mailbox;
      N       : Positive);
   --  Detele message number N from the mailbox

   function Get_Header
     (Mailbox : POP.Mailbox;
      N       : Positive) return Message;
   --  Retrieve headers for the Nth message from the mailbox, let the message
   --  on the mailbox. This is useful to build a quick summary of the mailbox.

   generic
      with procedure Action
        (Message : POP.Message;
         Index   : Positive;
         Quit    : in out Boolean);
   procedure For_Every_Message
     (Mailbox : POP.Mailbox;
      Remove  : Boolean := False);
   --  Calls Action for each message read on the mailbox, delete the message
   --  from the mailbox if Remove is True. Set Quit to True to stop the
   --  iterator. Index is the mailbox's message index.

   generic
      with procedure Action
        (Message : POP.Message;
         Index   : Positive;
         Quit    : in out Boolean);
   procedure For_Every_Message_Header (Mailbox : POP.Mailbox);
   --  Calls Action for each message read on the mailbox. Only the headers are
   --  read from the mailbox. Set Quit to True to stop the iterator. Index is
   --  the mailbox's message index.

   function Size (Message : POP.Message) return Natural;
   --  Returns the message size in bytes

   function Content (Message : POP.Message) return Unbounded_String;
   --  Returns message's content as an Unbounded_String. Each line are
   --  separated by CR+LF characters.

   function From (Message : POP.Message) return String;
   --  Returns From header value

   function To (Message : POP.Message; N : Natural := 0) return String;
   --  Returns the To header value. If N = 0 returns all recipients separated
   --  by a coma otherwise it returns the Nth To recipient.

   function To_Count (Message : POP.Message) return Natural;
   --  Returns the number of To recipient for Message. returns 0 if there is
   --  no To for this message.

   function CC (Message : POP.Message; N : Natural := 0) return String;
   --  Retruns the CC header value. If N = 0 returns all recipients separated
   --  by a coma otherwise it returns the Nth CC recipient.

   function CC_Count (Message : POP.Message) return Natural;
   --  Returns the number of CC recipient for Message. Returns 0 if there is
   --  no CC for this message.

   function Subject (Message : POP.Message) return String;
   --  Returns Subject header value

   function Date (Message : POP.Message) return String;
   --  Returns Date header value

   function Header
     (Message : POP.Message;
      Header  : String) return String;
   --  Returns header value for header named Header, returns the empty string
   --  if such header does not exist.

   ----------------
   -- Attachment --
   ----------------

   type Attachment is private;

   function Attachment_Count (Message : POP.Message) return Natural;
   --  Returns the number of Attachments into Message

   function Get
     (Message : POP.Message'Class;
      Index   : Positive) return Attachment;
   --  Returns the Nth Attachment for Message, Raises Constraint_Error if
   --  there is not such attachment.

   generic
      with procedure Action
        (Attachment : POP.Attachment;
         Index      : Positive;
         Quit       : in out Boolean);
   procedure For_Every_Attachment (Message : POP.Message);
   --  Calls action for every Attachment in Message. Stop iterator if Quit is
   --  set to True, Quit is set to False by default.

   function Content
     (Attachment : POP.Attachment)
      return AWS.Resources.Streams.Stream_Access;
   --  Returns Attachment's content as a memory stream. Note that the stream
   --  has already been decoded. Most attachments are MIME Base64 encoded.

   function Content (Attachment : POP.Attachment) return Unbounded_String;
   --  Returns Attachment's content as an Unbounded_String. This routine must
   --  only be used for non file attachments. Raises Constraint_Error if
   --  called for a file attachment.

   function Filename (Attachment : POP.Attachment) return String;
   --  Returns the Attachment filename or the empty string if it is not a file
   --  but an embedded message.

   function Is_File (Attachment : POP.Attachment) return Boolean;
   --  Returns True if Attachment is a file

   procedure Write (Attachment : POP.Attachment; Directory : String);
   --  Writes Attachment's file content into Directory. This must only be used
   --  for a file attachment.

private

   use Ada;

   -------------
   -- Mailbox --
   -------------

   type Mailbox is record
      Sock          : Net.Std.Socket_Type;
      Name          : Unbounded_String;
      User_Name     : Unbounded_String;
      Message_Count : Natural;
      Size          : Natural;
   end record;

   type Attachment_Access is access Attachment;

   -------------
   -- Message --
   -------------

   type Message is new Finalization.Controlled with record
      Ref_Count   : AWS.Utils.Counter_Access;
      Size        : Natural;
      Headers     : AWS.Headers.List;
      Content     : Unbounded_String;
      Attachments : Attachment_Access;
      Last        : Attachment_Access;
   end record;

   overriding procedure Initialize (Message : in out POP.Message);
   overriding procedure Adjust     (Message : in out POP.Message);
   overriding procedure Finalize   (Message : in out POP.Message);

   ----------------
   -- Attachment --
   ----------------

   type Attachment is new Finalization.Controlled with record
      Ref_Count : AWS.Utils.Counter_Access;
      Headers   : AWS.Headers.List;
      Content   : AWS.Resources.Streams.Stream_Access;
      Filename  : Unbounded_String;
      Next      : Attachment_Access;
   end record;

   overriding procedure Initialize (Attachment : in out POP.Attachment);
   overriding procedure Adjust     (Attachment : in out POP.Attachment);
   overriding procedure Finalize   (Attachment : in out POP.Attachment);

end AWS.POP;
