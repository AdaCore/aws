------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                               ACT-Europe                                 --
--                                                                          --
--  Authors: Dmitriy Anisimov - Pascal Obry                                 --
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

with Ada.Streams;
with Ada.Strings.Unbounded;
with Sockets;

with AWS.Response;
with AWS.URL;

package AWS.Client is

   URL_Error : exception;

   No_Data       : constant String := "";

   Retry_Default : constant := 1;
   --  Number of time a data is requested from the Server if the connection
   --  fails.

   function Get
     (URL        : in String;
      User       : in String := No_Data;
      Pwd        : in String := No_Data;
      Proxy      : in String := No_Data;
      Proxy_User : in String := No_Data;
      Proxy_Pwd  : in String := No_Data)
     return Response.Data;
   --  retreive the message data given a specific URL. It open a connection
   --  with the server and ask for the ressource specified in the URL it then
   --  return it in the Response.Data structure.
   --  If User/Pwd are given then it uses it to access the URL.
   --
   --  Eventually it connect through a PROXY using if necessary the Proxy
   --  authentification Proxy_User:Proxy_Pwd.
   --
   --  Only Basic authetification is supported (i.e. Digest is not).
   --
   --  Get will retry one time if it fails.

   function Head
     (URL        : in String;
      User       : in String := No_Data;
      Pwd        : in String := No_Data;
      Proxy      : in String := No_Data;
      Proxy_User : in String := No_Data;
      Proxy_Pwd  : in String := No_Data)
     return Response.Data;
   --  Idem as above but we do not get the message body.
   --  Head will retry one time if it fails.

   function Put
     (URL        : in String;
      Data       : in String;
      User       : in String := No_Data;
      Pwd        : in String := No_Data;
      Proxy      : in String := No_Data;
      Proxy_User : in String := No_Data;
      Proxy_Pwd  : in String := No_Data)
     return Response.Data;
   --  Send to the server URL a PUT request with Data
   --  Put will retry one time if it fails.

   function Post
     (URL        : in String;
      Data       : in String;
      User       : in String := No_Data;
      Pwd        : in String := No_Data;
      Proxy      : in String := No_Data;
      Proxy_User : in String := No_Data;
      Proxy_Pwd  : in String := No_Data)
     return Response.Data;
   --  Send to the server URL a POST request with Data
   --  Post will retry one time if it fails.

   function Post
     (URL        : in String;
      Data       : in Ada.Streams.Stream_Element_Array;
      User       : in String := No_Data;
      Pwd        : in String := No_Data;
      Proxy      : in String := No_Data;
      Proxy_User : in String := No_Data;
      Proxy_Pwd  : in String := No_Data)
     return Response.Data;
   --  Idem as above but with binary data.

   --  Keep-Alive client implementation.

   type HTTP_Connection is private;

   function Create
     (Host       : in String;
      User       : in String   := No_Data;
      Pwd        : in String   := No_Data;
      Proxy      : in String   := No_Data;
      Proxy_User : in String   := No_Data;
      Proxy_Pwd  : in String   := No_Data;
      Retry      : in Positive := Retry_Default;
      Persistent : in Boolean  := True)
     return HTTP_Connection;
   --  Create a new connection. This is to be used with Keep-Alive client API
   --  below. The request will be tried Retry time if it fails.

   procedure Get
     (Connection : in out HTTP_Connection;
      Result     :    out Response.Data;
      URI        : in     String          := No_Data);
   --  Same as Get above but using a Connection.

   procedure Head
     (Connection : in out HTTP_Connection;
      Result     :    out Response.Data;
      URI        : in     String          := No_Data);
   --  Same as Head above but using a Connection.

   procedure Put
     (Connection : in out HTTP_Connection;
      Result     :    out Response.Data;
      Data       : in     String;
      URI       : in     String          := No_Data);
   --  Same as Put above but using a Connection.

   procedure Post
     (Connection : in out HTTP_Connection;
      Result     :    out Response.Data;
      Data       : in     String;
      URI        : in     String          := No_Data);
   --  Same as Post above but using a Connection.

   procedure Post
     (Connection : in out HTTP_Connection;
      Result     :    out Response.Data;
      Data       : in     Ada.Streams.Stream_Element_Array;
      URI        : in     String          := No_Data);
   --  Same as Post above but using a Connection.

   procedure Close (Connection : in out HTTP_Connection);
   --  Close connection, it releases all associated ressources.

private

   use Ada.Strings.Unbounded;

   type Socket_Access is access Sockets.Socket_FD'Class;

   type HTTP_Connection is record
      Connect_URL : AWS.URL.Object;
      Host        : Unbounded_String;
      Host_URL    : AWS.URL.Object;
      User        : Unbounded_String;
      Pwd         : Unbounded_String;
      Proxy       : Unbounded_String;
      Proxy_URL   : AWS.URL.Object;
      Proxy_User  : Unbounded_String;
      Proxy_Pwd   : Unbounded_String;
      Opened      : Boolean;
      Persistent  : Boolean;
      Socket      : Socket_Access;
      Retry       : Positive;
   end record;

end AWS.Client;
