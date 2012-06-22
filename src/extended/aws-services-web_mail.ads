------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2013, AdaCore                     --
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

with AWS.Response;
with AWS.Status;

package AWS.Services.Web_Mail is

   function Callback (Request : Status.Data) return Response.Data;
   --  This is the AWS callback for the simple static Web Mail server

end AWS.Services.Web_Mail;
