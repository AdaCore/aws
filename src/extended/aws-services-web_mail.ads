------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2009, AdaCore                     --
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

--  The Callback is an implementation of a simple Web Mail that works on a
--  POP mailbox. It is possible to customize this service using the following
--  template files:
--
--  wm_login.html     Simple HTML page that is used to login user to the
--                    POP server. Three input are required: WM_POP_SERVER (the
--                    name or IP address of the POP server), WM_USER_NAME (the
--                    user name or mailbox name), WM_PASSWORD.
--
--  wm_summary.thtml  A template to display the mailbox summary. The following
--                    variable tags are defined:
--                    WM_POP_SERVER    name of the POP server
--                    WM_USER_NAME     user or mailbox name
--                    WM_MESS_COUNT    total number of messages in mailbox
--                    WM_MAILBOX_SIZE  the number of bytes into the mailbox
--                    WM_MESSAGE_V     message numbers (vector tag)
--                    WM_SIZE_V        corresponding merssage size
--                    WM_FROM_V        corresponding from field
--                    WM_DATE_V        corresponding data field
--                    WM_SUBJECT_V     corresponding subject field
--
--  wm_message.thtml  A template to display a message. The following variable
--                    tags are defined:
--                    WM_POP_SERVER    name of the POP server
--                    WM_USER_NAME     user or mailbox name
--                    WM_MESS_COUNT    total number of messages in mailbox
--                    WM_MESSAGE       message number
--                    WM_DATE          date of the message
--                    WM_FROM          sender address
--                    WM_CC            carbon-copy recipients
--                    WM_SUBJECT       message's subject
--                    WM_CONTENT       message's content
--                    WM_ATT_NAME_V    message attachment names (vector tag)
--                    WM_ATT_REF_V     URI reference to the attachment content
--
--  wm_reply.thtml    A template to reply to a given message. The following
--                    variable tags are defined:
--                    WM_SMTP_SERVER   server that will be used to send message
--                    WM_POP_SERVER    name of The POP Server
--                    WM_USER_NAME     user or mailbox name
--                    WM_MESS_COUNT    total number of messages in mailbox
--                    WM_MESSAGE       message number
--                    WM_DATE          date of the message
--                    WM_FROM          sender address
--                    WM_CC            carbon-copy recipients
--                    WM_SUBJECT       message's subject
--                    WM_CONTENT       message's content (lines prefix >)
--                    WM_TO_ALL        set to True in reply all case
--
--  In all templates the tag AWS_VERSION is defined and corresponds to the AWS
--  version string.

with AWS.Status;
with AWS.Response;

package AWS.Services.Web_Mail is

   function Callback (Request : AWS.Status.Data) return AWS.Response.Data;
   --  This is the AWS callback for the simple static Web Mail server

end AWS.Services.Web_Mail;
