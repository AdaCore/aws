------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                       P O P - Post Office Protocol                       --
--                                                                          --
--                            Copyright (C) 2003                            --
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

with Ada.Finalization;
with Ada.Strings.Unbounded;

with AWS.Headers;
with AWS.Net.Std;
with AWS.Resources.Streams.Memory;

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
     (Server_Name  : in String;
      User         : in String;
      Password     : in String;
      Authenticate : in Authenticate_Mode := Clear_Text;
      Port         : in Positive          := Default_POP_Port)
      return Mailbox;
   --  Connect on the given Port to Server_Name and open User's Mailbox. This
   --  mailbox object will be used to retrieve messages.

   procedure Close (Mailbox : in POP.Mailbox);
   --  Close mailbox

   function User_Name (Mailbox : in POP.Mailbox) return String;
   --  Returns User's name for this mailbox

   function Message_Count (Mailbox : in POP.Mailbox) return Natural;
   --  Returns the number of messages in the user's mailbox

   function Size (Mailbox : in POP.Mailbox) return Natural;
   --  Returns the total size in bytes of the user's mailbox

   -------------
   -- Message --
   -------------

   type Message is tagged private;

   function Get
     (Mailbox : in POP.Mailbox;
      N       : in Positive;
      Remove  : in Boolean     := False)
      return Message;
   --  Retrieve Nth message from the mailbox, let the message on the mailbox
   --  if Remove is False.

   generic
      with procedure Action
        (Message : in     POP.Message;
         Index   : in     Positive;
         Quit    : in out Boolean);
   procedure For_Every_Message
     (Mailbox : in POP.Mailbox;
      Remove  : in Boolean := False);
   --  Calls Action for each message read on the mailbox, delete the message
   --  from the mailbox if Remove is True. Set Quit to True to stop the
   --  iterator. Index is the mailbox's message index.

   function Content (Message : in POP.Message) return Unbounded_String;
   --  Returns message's content as an Unbounded_String. Each line are
   --  separated by CR+LF characters.

   function From (Message : in POP.Message) return String;
   --  Returns From header value

   function Subject (Message : in POP.Message) return String;
   --  Returns Subject header value

   function Date (Message : in POP.Message) return String;
   --  Returns Date header value

   function Header
     (Message : in POP.Message;
      Header  : in String)
      return String;
   --  Returns header value for header named Header, returns the empty string
   --  if such header does not exist.

   ----------------
   -- Attachment --
   ----------------

   type Attachment is private;

   function Attachment_Count (Message : in POP.Message) return Natural;
   --  Returns the number of Attachments into Message

   function Get
     (Message    : in POP.Message'Class;
      Attachment : in Positive)
      return Attachment;
   --  Returns the Nth Attachment for Message, Raises Constraint_Error if
   --  there is not such attachment.

   generic
      with procedure Action
        (Attachment : in     POP.Attachment;
         Quit       : in out Boolean);
   procedure For_Every_Attachment (Message : in POP.Message);
   --  Calls action for every Attachment in Message. Stop iterator if Quit is
   --  set to True, Quit is set to False by default.

   subtype Stream_Type is AWS.Resources.Streams.Memory.Stream_Type;

   function Content (Attachment : in POP.Attachment) return Stream_Type;
   --  Returns Attachment's content as a memory stream. Note that the stream
   --  has already been decoded. Most attachments are MIME Base64 encoded.

   function Content (Attachment : in POP.Attachment) return Unbounded_String;
   --  Returns Attachment's content as an Unbounded_String. This routine must
   --  only be used for non file attachments. Raises Constraint_Error if
   --  called for a file attachment.

   function Filename (Attachment : in POP.Attachment) return String;
   --  Returns the Attachment filename or the empty string if it is not a file
   --  but an embedded message.

   function Is_File (Attachment : in POP.Attachment) return Boolean;
   --  Returns True if Attachment is a file

   procedure Write (Attachment : in POP.Attachment; Directory : in String);
   --  Writes Attachment's file content into Directory. This must only be used
   --  for a file attachment.

private

   use Ada;

   type Mailbox is record
      Sock          : Net.Std.Socket_Type;
      Name          : Unbounded_String;
      User_Name     : Unbounded_String;
      Message_Count : Natural;
      Size          : Natural;
   end record;

   type Count_Access is access Natural;

   type Attachment_Access is access Attachment;

   type Message is new Finalization.Controlled with record
      Ref_Count   : Count_Access;
      Headers     : AWS.Headers.List;
      Content     : Unbounded_String;
      Attachments : Attachment_Access;
      Last        : Attachment_Access;
   end record;

   procedure Initialize (Message : in out POP.Message);
   procedure Adjust     (Message : in out POP.Message);
   procedure Finalize   (Message : in out POP.Message);

   type Attachment is new Finalization.Controlled with record
      Ref_Count : Count_Access;
      Headers   : AWS.Headers.List;
      Content   : AWS.Resources.Streams.Stream_Access;
      Filename  : Unbounded_String;
      Next      : Attachment_Access;
   end record;

   procedure Initialize (Attachment : in out POP.Attachment);
   procedure Adjust     (Attachment : in out POP.Attachment);
   procedure Finalize   (Attachment : in out POP.Attachment);

end AWS.POP;
