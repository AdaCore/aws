------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2002                          --
--                               ACT-Europe                                 --
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

with Ada.Calendar;
with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Streams.Stream_IO;
with Ada.Unchecked_Deallocation;

with GNAT.Table;
with GNAT.Calendar.Time_IO;

with AWS.Digest;
with AWS.Messages;
with AWS.MIME;
with AWS.Net.Buffered;
with AWS.OS_Lib;
with AWS.Response.Set;
with AWS.Translator;
with AWS.Utils;

package body AWS.Client is

   use Ada;
   use Ada.Strings.Unbounded;

   type Auth_Attempts_Count is
     array (Authentication_Level) of Natural range 0 .. 2;

   Debug_On    : Boolean := False;

   End_Section : constant String := "";

   procedure Debug_Message (Prefix, Message : in String);
   pragma Inline (Debug_Message);
   --  Output Message prefixed with Prefix if Debug_On is True and does
   --  nothing otherwise.

   procedure Get_Response
     (Connection : in out HTTP_Connection;
      Result     :    out Response.Data;
      Get_Body   : in     Boolean         := True);
   --  Receives response from server for GET and POST and HEAD commands.
   --  If Get_Body is set then the message body will be read.

   procedure Decrement_Authentication_Attempt
     (Connection : in out HTTP_Connection;
      Counter    : in out Auth_Attempts_Count;
      Over       :    out Boolean);
   --  Counts the authentication attempts. Over is set to True when
   --  authentication attempts are over.

   procedure Set_Authentication
     (Auth       :    out Authentication_Type;
      User       : in     String;
      Pwd        : in     String;
      Mode       : in     Authentication_Mode);
   --  Internal procedure to set authentication parameters.

   procedure Parse_Header
     (Connection        : in out HTTP_Connection;
      Status            :    out Messages.Status_Code;
      Content_Length    :    out Natural;
      Content_Type      :    out Unbounded_String;
      Transfer_Encoding :    out Unbounded_String;
      Location          :    out Unbounded_String;
      Keep_Alive        :    out Boolean);
   --  Read server answer and set corresponding variable with the value
   --  read. Most of the fields are ignored right now.

   procedure Disconnect (Connection : in out HTTP_Connection);
   --  Close connection. Further use is not possible.

   procedure Open_Send_Common_Header
     (Connection : in out HTTP_Connection;
      Method     : in     String;
      URI        : in     String);
   --  Open the the Connection if it is not open. Send the common HTTP headers
   --  for all requests like the proxy, authentification, user agent, host.

   procedure Set_Phase
     (Connection : in out HTTP_Connection;
      Phase      : in     Client_Phase);
   pragma Inline (Set_Phase);
   --  Set the phase for the connection. This will activate the Send and
   --  Receive timeouts of the cleaner task if needed.

   procedure Send_Header
     (Sock : in Net.Socket_Type'Class;
      Data : in String);
   pragma Inline (Send_Header);
   --  Send header Data to socket and call Debug_Message.

   procedure Free is new Ada.Unchecked_Deallocation
     (Net.Socket_Type'Class, Net.Socket_Access);

   ------------------
   -- Cleaner_Task --
   ------------------

   task body Cleaner_Task is
      Connection : HTTP_Connection_Access;
      Forever    : constant Duration := Duration'Last;
      P          : Client_Phase      := Not_Monitored;
      W          : Duration;
      Timeout    : Boolean;
   begin
      accept Start (Connection : in HTTP_Connection_Access) do
         Cleaner_Task.Connection := Connection;
      end Start;

      Phase_Loop : loop

         --  Wait for the job to be done

         case P is
            when Stopped =>
               exit Phase_Loop;

            when Not_Monitored =>
               W := Forever;

            when Receive =>
               W := Duration (Connection.Timeouts.Receive);

            when Send =>
               W := Duration (Connection.Timeouts.Send);
         end case;

         if W = 0.0 then
            P := Not_Monitored;
            W := Forever;
         end if;

         select
            accept Next_Phase do
               P       := Connection.Current_Phase;
               Timeout := False;
            end Next_Phase;
         or
            delay W;

            Timeout := True;
         end select;

         --  Still in the same phase after the delay, just close the socket
         --  now.

         if Timeout
           and then P /= Not_Monitored
           and then Connection.Opened
         then
            Disconnect (Connection.all);
         end if;

      end loop Phase_Loop;

   exception
      when E : others =>
         Text_IO.Put_Line (Exceptions.Exception_Information (E));
   end Cleaner_Task;

   -----------
   -- Close --
   -----------

   procedure Close (Connection : in out HTTP_Connection) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Cleaner_Task, Cleaner_Access);

   begin
      Connection.Current_Phase := Stopped;

      if not (Connection.Cleaner = null) then

         begin
            --  We don't want to fail here, we really want to free the cleaner
            --  object.
            if not Connection.Cleaner'Terminated then
               Connection.Cleaner.Next_Phase;
            end if;
         exception
            when others =>
               null;
         end;

         while not Connection.Cleaner'Terminated loop
            delay 0.01;
         end loop;

         Free (Connection.Cleaner);

      end if;

      Disconnect (Connection);
      Free (Connection.Socket);
   end Close;

   -----------------
   -- Copy_Cookie --
   -----------------

   procedure Copy_Cookie
     (Source      : in     HTTP_Connection;
      Destination : in out HTTP_Connection) is
   begin
      Destination.Cookie := Source.Cookie;
   end Copy_Cookie;

   ------------
   -- Create --
   ------------

   procedure Create
     (Connection  : in out HTTP_Connection;
      Host        : in     String;
      User        : in     String          := No_Data;
      Pwd         : in     String          := No_Data;
      Proxy       : in     String          := No_Data;
      Proxy_User  : in     String          := No_Data;
      Proxy_Pwd   : in     String          := No_Data;
      Retry       : in     Natural         := Retry_Default;
      SOAPAction  : in     String          := No_Data;
      Persistent  : in     Boolean         := True;
      Timeouts    : in     Timeouts_Values := No_Timeout;
      Server_Push : in     Boolean         := False)
   is
      function Set (V : in String) return Unbounded_String;
      --  Returns V as an Unbounded_String if V is not the empty string
      --  otherwise it returns Null_Unbounded_String.

      ---------
      -- Set --
      ---------

      function Set (V : in String) return Unbounded_String is
      begin
         if V = No_Data then
            return Null_Unbounded_String;
         else
            return To_Unbounded_String (V);
         end if;
      end Set;

      Connect_URL : AWS.URL.Object;
      Host_URL    : AWS.URL.Object := AWS.URL.Parse (Host);
      Proxy_URL   : AWS.URL.Object := AWS.URL.Parse (Proxy);

   begin
      --  If there is a proxy, the host to connect to is the proxy otherwise
      --  we connect to the Web server.

      if Proxy = No_Data then
         Connect_URL := Host_URL;
      else
         Connect_URL := Proxy_URL;
      end if;

      Connection.Host                     := To_Unbounded_String (Host);
      Connection.Host_URL                 := Host_URL;
      Connection.Connect_URL              := Connect_URL;
      Connection.Auth (WWW).User          := Set (User);
      Connection.Auth (WWW).Pwd           := Set (Pwd);
      Connection.Proxy                    := Set (Proxy);
      Connection.Proxy_URL                := Proxy_URL;
      Connection.Auth (Client.Proxy).User := Set (Proxy_User);
      Connection.Auth (Client.Proxy).Pwd  := Set (Proxy_Pwd);

      begin
         Connection.Socket := Net.Socket (AWS.URL.Security (Connect_URL));

         Net.Connect (Connection.Socket.all,
                      AWS.URL.Host (Connect_URL),
                      AWS.URL.Port (Connect_URL));
      exception
         when E : others =>
            Connection.Opened := False;

            Exceptions.Raise_Exception
              (Connection_Error'Identity,
               "can't connect to " & AWS.URL.URL (Connect_URL)
                 & " -> " & Exceptions.Exception_Information (E));
      end;

      Connection.Opened          := True;
      Connection.Retry           := Create.Retry;
      Connection.Cookie          := Null_Unbounded_String;
      Connection.SOAPAction      := Set (SOAPAction);
      Connection.Persistent      := Persistent;
      Connection.Current_Phase   := Not_Monitored;
      Connection.Server_Push     := Server_Push;

      if Persistent and then Connection.Retry = 0 then
         --  In this case the connection termination can be initiated by the
         --  server or the client after a period. So the connection could be
         --  closed while trying to get some data from the server. To be nicer
         --  from user's point of view just make sure we retry at least one
         --  time before reporting an error.
         Connection.Retry := 1;
      end if;

      Connection.Timeouts := Timeouts;

      if Connection.Timeouts /= No_Timeout then
         --  If we have some timeouts, initialize the cleaner task.
         Connection.Cleaner := new Cleaner_Task;
         Connection.Cleaner.Start (Connection.Self);
      end if;
   end Create;

   -------------------
   -- Debug_Message --
   -------------------

   procedure Debug_Message (Prefix, Message : in String) is
   begin
      if Debug_On then
         Text_IO.Put_Line (Prefix & Message);
      end if;
   end Debug_Message;

   --------------------------------------
   -- Decrement_Authentication_Attempt --
   --------------------------------------

   procedure Decrement_Authentication_Attempt
     (Connection : in out HTTP_Connection;
      Counter    : in out Auth_Attempts_Count;
      Over       :    out Boolean)
   is
      type Over_Data is array (Authentication_Level) of Boolean;

      Is_Over    : constant Over_Data := (others => True);
      Over_Level : Over_Data          := (others => True);
   begin
      for Level in Authentication_Level'Range loop
         if Connection.Auth (Level).Requested then
            Counter (Level)    := Counter (Level) - 1;
            Over_Level (Level) := Counter (Level) = 0;
         end if;
      end loop;

      Over := Over_Level = Is_Over;
   end Decrement_Authentication_Attempt;

   ----------------
   -- Disconnect --
   ----------------

   procedure Disconnect (Connection : in out HTTP_Connection) is
      use type Net.Socket_Access;
   begin
      if Connection.Opened then
         Connection.Opened := False;

         if Connection.Socket /= null then
            Net.Buffered.Shutdown (Connection.Socket.all);
         end if;
      end if;
   end Disconnect;

   ---------
   -- Get --
   ---------

   function Get
     (URL                : in String;
      User               : in String          := No_Data;
      Pwd                : in String          := No_Data;
      Proxy              : in String          := No_Data;
      Proxy_User         : in String          := No_Data;
      Proxy_Pwd          : in String          := No_Data;
      Timeouts           : in Timeouts_Values := No_Timeout;
      Follow_Redirection : in Boolean         := False)
      return Response.Data
   is
      use type Messages.Status_Code;

      Connection : HTTP_Connection;
      Result     : Response.Data;

   begin
      Create (Connection,
              URL, User, Pwd, Proxy, Proxy_User, Proxy_Pwd,
              Persistent => False,
              Timeouts   => Timeouts);

      Get (Connection, Result);

      Close (Connection);

      if Follow_Redirection
        and then
        Response.Status_Code (Result) = Messages.S305
      then
         --  This is "Use Proxy" message, Location point to the proxy to use.
         --  We do not have the login/password for the proxy.
         return Get
           (URL, User, Pwd, Response.Location (Result),
            Timeouts => Timeouts, Follow_Redirection => Follow_Redirection);

      elsif Follow_Redirection
        and then
        Response.Status_Code (Result) in Messages.S301 .. Messages.S307
        and then
        Response.Status_Code (Result) /= Messages.S304
      then
         --  All other redirections, 304 is not one of them.
         return Get
           (Response.Location (Result), User, Pwd,
            Proxy, Proxy_User, Proxy_Pwd, Timeouts, Follow_Redirection);
      else
         return Result;
      end if;

   exception
      when others =>
         Close (Connection);
         raise;
   end Get;

   ---------
   -- Get --
   ---------

   procedure Get
     (Connection : in out HTTP_Connection;
      Result     :    out Response.Data;
      URI        : in     String          := No_Data)
   is
      Try_Count     : Natural := Connection.Retry;

      Auth_Attempts : Auth_Attempts_Count := (others => 2);
      Auth_Is_Over  : Boolean;

   begin

      loop
         begin
            Open_Send_Common_Header (Connection, "GET", URI);

            Net.Buffered.New_Line (Connection.Socket.all);

            Get_Response (Connection, Result, not Connection.Server_Push);

            Decrement_Authentication_Attempt
              (Connection, Auth_Attempts, Auth_Is_Over);

            if Auth_Is_Over then
               return;
            end if;

         exception
            when Net.Socket_Error =>

               Disconnect (Connection);

               if Try_Count = 0 then
                  Result := Response.Build
                    (MIME.Text_HTML, "Get Timeout", Messages.S408);

                  Set_Phase (Connection, Not_Monitored);
                  exit;
               end if;

               Try_Count := Try_Count - 1;
         end;
      end loop;
   end Get;

   ------------------
   -- Get_Response --
   ------------------

   procedure Get_Response
     (Connection : in out HTTP_Connection;
      Result     :    out Response.Data;
      Get_Body   : in     Boolean         := True)
   is
      type Stream_Element_Array_Access is access Streams.Stream_Element_Array;

      procedure Check_Status;
      --  check for special status

      function Read_Chunk return Streams.Stream_Element_Array;
      --  Read a chunk object from the stream

      function Read_Binary_Message
        (Len : in Positive)
        return Stream_Element_Array_Access;
      pragma Inline (Read_Binary_Message);
      --  Read a binary message of Len bytes from the socket.

      function Read_Message return String;
      --  Read a textual message from the socket for which there is no known
      --  length.

      procedure Disconnect;
      --  close connection socket.

      procedure Free is new Ada.Unchecked_Deallocation
        (Streams.Stream_Element_Array, Stream_Element_Array_Access);

      Sock       : Net.Socket_Type'Class renames Connection.Socket.all;

      CT         : Unbounded_String;
      CT_Len     : Natural              := 0;
      TE         : Unbounded_String;
      Location   : Unbounded_String;
      Status     : Messages.Status_Code;

      Keep_Alive : Boolean;

      ------------------
      -- Check_Status --
      ------------------

      procedure Check_Status is
         use type Messages.Status_Code;
      begin
         if Status in Messages.S301 .. Messages.S307
           and then
           Status /= Messages.S304
         then
            --  Page Moved, Location pointing to new location

            Result := Response.Moved
              (Location => To_String (Location),
               Message  => Response.Message_Body (Result));

         elsif Status = Messages.S404 then

            if String'(Response.Message_Body (Result)) = "" then
               Result := Response.Build
                 (MIME.Text_HTML, "(404) not found", Status);
            end if;

         end if;
      end Check_Status;

      ----------------
      -- Disconnect --
      ----------------

      procedure Disconnect is
      begin
         if not Keep_Alive
           and not Connection.Server_Push
         then
            Disconnect (Connection);
         end if;
      end Disconnect;

      -------------------------
      -- Read_Binary_Message --
      -------------------------

      function Read_Binary_Message
        (Len : in Positive)
         return Stream_Element_Array_Access
      is
         use Streams;

         Elements : Stream_Element_Array_Access
           := new Stream_Element_Array (1 .. Stream_Element_Offset (Len));
         S, E     : Stream_Element_Offset;
      begin
         S := 1;

         --  Read the message, 10k at a time

         loop
            E := Stream_Element_Offset'Min
              (Stream_Element_Offset (Len), S + 10_239);

            Net.Buffered.Read (Sock, Elements (S .. E));

            S := E + 1;

            exit when S > Stream_Element_Offset (Len);
         end loop;

         return Elements;

      exception
         when Net.Socket_Error =>
            --  Could have been killed by a timeout.
            Free (Elements);
            raise;
      end Read_Binary_Message;

      ----------------
      -- Read_Chunk --
      ----------------

      function Read_Chunk return Streams.Stream_Element_Array is

         use Streams;

         use type Stream_Element_Array;
         use type Stream_Element_Offset;

         procedure Skip_Line;
         --  skip a line on the socket

         Data      : Stream_Element_Array_Access
           := new Streams.Stream_Element_Array (1 .. 10_000);

         Data_Last : Streams.Stream_Element_Offset := 0;

         ---------------
         -- Skip_Line --
         ---------------

         procedure Skip_Line is
            D : constant String := Net.Buffered.Get_Line (Sock);
            pragma Warnings (Off, D);
         begin
            null;
         end Skip_Line;

         Size : Stream_Element_Offset;
         Tmp  : Stream_Element_Array_Access;

      begin
         loop
            --  Read the chunk size that is an hex number
            declare
               L : constant String := Net.Buffered.Get_Line (Sock);
            begin
               Size := Stream_Element_Offset
                 (Utils.Hex_Value (Strings.Fixed.Trim (L, Strings.Both)));
            end;

            if Size = 0 then
               Skip_Line;
               exit;

            else
               if Data_Last + Size > Data'Last then

                  Tmp := new Stream_Element_Array
                    (1 ..
                       Stream_Element_Offset'Max
                         (Data_Last + Size, 2 * Data'Length));

                  Tmp (1 .. Data_Last) := Data (1 .. Data_Last);
                  Free (Data);
                  Data := Tmp;
               end if;

               Net.Buffered.Read
                 (Sock, Data (Data_Last + 1 .. Data_Last + Size));

               Skip_Line;
               Data_Last := Data_Last + Size;
            end if;

         end loop;

         declare
            Copy : Stream_Element_Array (1 .. Data_Last);
         begin
            Copy := Data (1 .. Data_Last);
            Free (Data);
            return Copy;
         end;

      exception
         when others =>
            --  Could have been killed by a timeout.
            Free (Data);
            raise;
      end Read_Chunk;

      ------------------
      -- Read_Message --
      ------------------

      function Read_Message return String is
         Results : Unbounded_String;
      begin
         --  We don't know the message body length, so read the socket until
         --  it is closed by the server. At this time an exception will be
         --  raised as we are trying to read the socket.

         loop
            declare
               Part : constant Streams.Stream_Element_Array
                 := Net.Buffered.Read (Sock);
            begin
               Append (Results, Translator.To_String (Part));
            end;
         end loop;

         return To_String (Results);

      exception
         when Net.Socket_Error =>
            return To_String (Results);
      end Read_Message;

   begin
      Set_Phase (Connection, Receive);

      Parse_Header
        (Connection, Status, CT_Len, CT, TE, Location, Keep_Alive);

      if not Get_Body then
         Result := Response.Build (To_String (CT), "", Status);

         --  Force Response Data content length

         Response.Set.Content_Length (Result, CT_Len);

         Disconnect;
         Set_Phase (Connection, Not_Monitored);
         Check_Status;
         return;
      end if;

      --  Read the message body

      if To_String (TE) = "chunked" then

         --  A chuncked message is written on the stream as list of data
         --  chunk. Each chunk has the following format:
         --
         --  <N : the chunk size in hexadecimal> CRLF
         --  <N * BYTES : the data> CRLF
         --
         --  The termination chunk is:
         --
         --  0 CRLF
         --  CRLF
         --

         declare
            CT  : constant String := To_String (Get_Response.CT);
         begin
            if MIME.Is_Text (CT) then
               --  This is a textual chunked encoded body
               Result := Response.Build
                 (CT, Translator.To_String (Read_Chunk), Status);

            else
               --  This is really some kind of binary data
               Result := Response.Build (CT, Read_Chunk, Status);
            end if;
         end;

      else
         if CT_Len = 0 then
            if MIME.Is_Text (To_String (CT)) then
               --  Here we do not know the message body length, but this is a
               --  textual data, read it as a string.

               Result := Response.Build
                 (To_String (CT), Read_Message, Status);

            else
               declare

                  package Stream_Element_Table is new GNAT.Table
                    (Streams.Stream_Element, Natural, 1, 30_000, 10_000);

                  procedure Add (B : in Streams.Stream_Element_Array);
                  --  Add B to Data

                  procedure Read_Until_Close;
                  --  Read data on socket, stop when the socket is closed.

                  ---------
                  -- Add --
                  ---------

                  procedure Add (B : in Streams.Stream_Element_Array) is
                  begin
                     for K in B'Range loop
                        Stream_Element_Table.Increment_Last;
                        Stream_Element_Table.Table
                          (Stream_Element_Table.Last) := B (K);
                        --  ??? in GNAT 3.15 we will use:
                        --  Stream_Element_Table.Append (B (K));
                     end loop;
                  end Add;

                  ----------------------
                  -- Read_Until_Close --
                  ----------------------

                  procedure Read_Until_Close is
                  begin
                     loop
                        declare
                           Data : constant Streams.Stream_Element_Array
                             := Net.Buffered.Read (Sock);
                        begin
                           Add (Data);
                        end;
                     end loop;
                  exception
                     when Net.Socket_Error =>
                        null;
                  end Read_Until_Close;

               begin
                  Read_Until_Close;

                  Result := Response.Build
                    (To_String (CT),
                     Streams.Stream_Element_Array
                       (Stream_Element_Table.Table
                          (1 .. Stream_Element_Table.Last)),
                     Status);

                  Stream_Element_Table.Free;
               end;
            end if;

         else

            declare
               Elements : Stream_Element_Array_Access
                 := Read_Binary_Message (CT_Len);
            begin
               if MIME.Is_Text (To_String (CT)) then
                  Result :=  Response.Build
                    (To_String (CT),
                     Translator.To_String (Elements.all),
                     Status);

               else
                  --  This is some kind of binary data.

                  Result := Response.Build
                    (To_String (CT), Elements.all, Status);
               end if;

               Free (Elements);
            end;
         end if;
      end if;

      Check_Status;
      Disconnect;

      Set_Phase (Connection, Not_Monitored);
   end Get_Response;

   ----------
   -- Head --
   ----------

   function Head
     (URL        : in String;
      User       : in String          := No_Data;
      Pwd        : in String          := No_Data;
      Proxy      : in String          := No_Data;
      Proxy_User : in String          := No_Data;
      Proxy_Pwd  : in String          := No_Data;
      Timeouts   : in Timeouts_Values := No_Timeout)
     return Response.Data
   is
      Connection : HTTP_Connection;
      Result     : Response.Data;

   begin
      Create (Connection,
              URL, User, Pwd, Proxy, Proxy_User, Proxy_Pwd,
              Persistent => False,
              Timeouts   => Timeouts);

      Head (Connection, Result);
      Close (Connection);
      return Result;

   exception
      when others =>
         Close (Connection);
         raise;
   end Head;

   ----------
   -- Head --
   ----------

   procedure Head
     (Connection : in out HTTP_Connection;
      Result     :    out Response.Data;
      URI        : in     String := No_Data)
   is
      Try_Count : Natural := Connection.Retry;

      Auth_Attempts : Auth_Attempts_Count := (others => 2);
      Auth_Is_Over  : Boolean;
   begin

      loop
         begin

            Open_Send_Common_Header (Connection, "HEAD", URI);

            Net.Buffered.New_Line (Connection.Socket.all);

            Get_Response (Connection, Result, Get_Body => False);

            Decrement_Authentication_Attempt
              (Connection, Auth_Attempts, Auth_Is_Over);

            if Auth_Is_Over then
               return;
            end if;

         exception
            when Net.Socket_Error =>

               Disconnect (Connection);

               if Try_Count = 0 then
                  Result := Response.Build
                    (MIME.Text_HTML, "Head Timeout", Messages.S408);
                  Set_Phase (Connection, Not_Monitored);
                  exit;
               end if;

               Try_Count := Try_Count - 1;
         end;
      end loop;
   end Head;

   -----------------------------
   -- Open_Send_Common_Header --
   -----------------------------

   procedure Open_Send_Common_Header
     (Connection : in out HTTP_Connection;
      Method     : in     String;
      URI        : in     String)
   is
      No_Data : Unbounded_String renames Null_Unbounded_String;

      procedure Send_Authentication_Header
        (Token       : in     String;
         Data        : in out Authentication_Type);
      --  Send the authentication header for proxy or for server.

      function HTTP_Prefix (Security : in Boolean) return String;
      --  Returns "http://" or "https://" if Security is set to True.

      function Persistence return String;
      --  Returns "Keep-Alive" is we have a persistent connection and "Close"
      --  otherwise.

      function Port_Not_Default (Port : in Positive) return String;
      --  Returns the port image (preceded by character ':') if it is not the
      --  default port.

      -----------------
      -- HTTP_Prefix --
      -----------------

      function HTTP_Prefix (Security : in Boolean) return String is
      begin
         if Security then
            return "https://";
         else
            return "http://";
         end if;
      end HTTP_Prefix;

      -----------------
      -- Persistence --
      -----------------

      function Persistence return String is
      begin
         if Connection.Persistent then
            return "Keep-Alive";
         else
            return "Close";
         end if;
      end Persistence;

      ----------------------
      -- Port_Not_Default --
      ----------------------

      function Port_Not_Default (Port : in Positive) return String is
      begin
         if Port = 80 then
            return "";
         else
            declare
               Port_Image : constant String := Positive'Image (Port);
            begin
               return ':' & Port_Image (2 .. Port_Image'Last);
            end;
         end if;
      end Port_Not_Default;

      --------------------------------
      -- Send_Authentication_Header --
      --------------------------------

      procedure Send_Authentication_Header
        (Token : in     String;
         Data  : in out Authentication_Type)
      is

         Username : constant String := To_String (Data.User);

      begin
         if Data.User /= No_Data
           and then Data.Pwd /= No_Data
         then
            if Data.Work_Mode = Basic then
               Send_Header
                 (Connection.Socket.all,
                  Token & "Basic "
                    & AWS.Translator.Base64_Encode
                    (Username
                       & ':' & To_String (Data.Pwd)));

            elsif Data.Work_Mode = Digest then

               declare

                  Nonce    : constant String := To_String (Data.Nonce);
                  Realm    : constant String := To_String (Data.Realm);
                  QOP      : constant String := To_String (Data.QOP);

                  function Get_URI return String;
                  --  Returns the real URI where the request is going to be
                  --  sent. It is either Open_Send_Common_Header.URI parameter
                  --  if it exists (without the HTTP parameters part), or URI
                  --  part of the Connection.Connect_URL field.

                  function QOP_Data return String;
                  --  Returns string with qop, cnonce and nc parameters
                  --  if qop parameter exists in the server auth request,
                  --  or empty string if not (RFC 2617 3.2.2).

                  Response : AWS.Digest.Digest_String;

                  -------------
                  -- Get_URI --
                  -------------

                  function Get_URI return String is
                     URI_Last : Natural;
                  begin
                     if URI = "" then
                        return URL.Path (Connection.Connect_URL)
                          & URL.File (Connection.Connect_URL);
                     else
                        URI_Last := Strings.Fixed.Index (URI, "?");

                        if URI_Last = 0 then
                           URI_Last := URI'Last;
                        else
                           URI_Last := URI_Last - 1;
                        end if;

                        return URI (URI'First .. URI_Last);
                     end if;
                  end Get_URI;

                  URI : constant String := Get_URI;

                  --------------
                  -- QOP_Data --
                  --------------

                  function QOP_Data return String is
                     CNonce : constant String := AWS.Digest.Create_Nonce;
                  begin
                     if QOP = No_Data then
                        Response := AWS.Digest.Create_Digest
                          (Username => Username,
                           Realm    => Realm,
                           Password => To_String (Data.Pwd),
                           Nonce    => Nonce,
                           Method   => Method,
                           URI      => URI);
                        return "";

                     else
                        Data.NC := Data.NC + 1;

                        declare
                           NC : constant String := Utils.Hex (Data.NC, 8);
                        begin
                           Response := AWS.Digest.Create_Digest
                             (Username => Username,
                              Realm    => Realm,
                              Password => To_String (Data.Pwd),
                              Nonce    => Nonce,
                              CNonce   => CNonce,
                              NC       => NC,
                              QOP      => QOP,
                              Method   => Method,
                              URI      => URI);

                           return "qop=""" & QOP
                             & """, cnonce=""" & CNonce
                             & """, nc=" & NC
                             & ", ";
                        end;
                     end if;
                  end QOP_Data;

               begin
                  Send_Header
                    (Connection.Socket.all,
                     Token & "Digest "
                       & QOP_Data
                       & "nonce=""" & Nonce
                       & """, username=""" & Username
                       & """, realm=""" & Realm
                       & """, uri=""" & URI
                       & """, response=""" & Response
                       & """");
               end;

            end if;
         end if;
      end Send_Authentication_Header;

      Host_Address : constant String
        := AWS.URL.Host (Connection.Host_URL)
        & Port_Not_Default (AWS.URL.Port (Connection.Host_URL));

   begin
      --  Open socket if needed.

      if not Connection.Opened then
         Connection.Socket
           := Net.Socket (AWS.URL.Security (Connection.Connect_URL));

         Net.Connect
           (Connection.Socket.all,
            AWS.URL.Host (Connection.Connect_URL),
            AWS.URL.Port (Connection.Connect_URL));

         Connection.Opened := True;
      end if;

      Set_Phase (Connection, Send);

      --  Header command.

      if Connection.Proxy = No_Data then

         if URI = "" then
            Send_Header
              (Connection.Socket.all,
               Method & ' '
                 & AWS.URL.Pathname_And_Parameters (Connection.Host_URL, False)
                 & ' ' & HTTP_Version);
         else
            --  URI should already be encoded, but to help a bit Windows
            --  systems who tend to have spaces into URL we encode them here.

            declare
               E_URI : String := URI;
            begin
               for K in E_URI'Range loop
                  if E_URI (K) = ' ' then
                     E_URI (K) := '+';
                  end if;
               end loop;

               Send_Header
                 (Connection.Socket.all,
                  Method & ' ' & E_URI & ' ' & HTTP_Version);
            end;
         end if;

         Send_Header
           (Connection.Socket.all, Messages.Connection (Persistence));

      else
         if URI = "" then
            Send_Header (Connection.Socket.all,
                         Method & ' '
                           & To_String (Connection.Host)
                           & ' ' & HTTP_Version);
         else
            Send_Header
              (Connection.Socket.all,
               Method & ' '
                 & HTTP_Prefix (AWS.URL.Security (Connection.Host_URL))
                 & Host_Address & URI
                 & ' ' & HTTP_Version);
         end if;

         Send_Header
           (Connection.Socket.all, Messages.Proxy_Connection (Persistence));

      end if;

      --  Cookie

      if Connection.Cookie /= No_Data then
         Send_Header
           (Connection.Socket.all,
            Messages.Cookie_Token & To_String (Connection.Cookie));
      end if;

      Send_Header (Connection.Socket.all,
                   Messages.Host (Host_Address));

      Send_Header (Connection.Socket.all,
                   Messages.Accept_Type ("text/html, */*"));

      Send_Header (Connection.Socket.all,
                   Messages.Accept_Language ("fr, us"));

      Send_Header (Connection.Socket.all,
                   Messages.User_Agent ("AWS (Ada Web Server) v" & Version));

      --  User Authentification

      Send_Authentication_Header
        (Messages.Authorization_Token, Connection.Auth (WWW));

      --  Proxy Authentification

      Send_Authentication_Header
        (Messages.Proxy_Authorization_Token, Connection.Auth (Proxy));

      --  SOAP header

      if Connection.SOAPAction /= No_Data then
         Send_Header
           (Connection.Socket.all,
            Messages.SOAPAction (To_String (Connection.SOAPAction)));
      end if;

      Set_Phase (Connection, Not_Monitored);
   end Open_Send_Common_Header;

   ------------------
   -- Parse_Header --
   ------------------

   procedure Parse_Header
     (Connection        : in out HTTP_Connection;
      Status            :    out Messages.Status_Code;
      Content_Length    :    out Natural;
      Content_Type      :    out Unbounded_String;
      Transfer_Encoding :    out Unbounded_String;
      Location          :    out Unbounded_String;
      Keep_Alive        :    out Boolean)
   is

      Sock : Net.Socket_Type'Class renames Connection.Socket.all;

      Request_Auth_Mode : array (Authentication_Level) of Authentication_Mode
        := (others => Any);

      procedure Parse_Authenticate_Line
        (Level     : in Authentication_Level;
         Auth_Line : in String);
      --  Parses Authentication request line and fill Connection.Auth (Level)
      --  field with the information read on the line. Handle WWW and Proxy
      --  authentication.

      procedure Set_Keep_Alive (Data : in String);
      --  Set the Parse_Header.Keep_Alive depending on data from the
      --  Proxy-Connection or Connection header line.

      -----------------------------
      -- Parse_Authenticate_Line --
      -----------------------------

      procedure Parse_Authenticate_Line
        (Level     : in Authentication_Level;
         Auth_Line : in     String)
      is
         Basic_Token  : constant String := "Basic ";
         Digest_Token : constant String := "Digest ";

         type Auth_Attribute is (Realm, Nonce, QOP, Algorithm);

         type Result_Set is array (Auth_Attribute) of Unbounded_String;

         Auth         : Authentication_Type renames Connection.Auth (Level);

         Result       : Result_Set;
         Request_Mode : Authentication_Mode;

         procedure Parse_Line is
            new AWS.Utils.Parse_HTTP_Header_Line (Auth_Attribute, Result_Set);

      begin
         if Messages.Match (Auth_Line, Basic_Token) then
            Request_Mode := Basic;

            Parse_Line
              (Auth_Line
                 (Auth_Line'First + Basic_Token'Length .. Auth_Line'Last),
               Result);

         elsif Messages.Match (Auth_Line, Digest_Token) then
            Request_Mode := Digest;

            Parse_Line
              (Auth_Line
                 (Auth_Line'First + Digest_Token'Length .. Auth_Line'Last),
               Result);

         else
            --  Ignore unrecognized authentication mode
            return;
         end if;

         if Request_Mode > Request_Auth_Mode (Level) then

            Auth.Requested := True;

            Request_Auth_Mode (Level) := Request_Mode;
            Auth.Work_Mode := Request_Mode;

            Auth.Realm := Result (Realm);
            Auth.Nonce := Result (Nonce);
            Auth.QOP   := Result (QOP);
            Auth.NC    := 0;

            if Result (Algorithm) /= Null_Unbounded_String
              and then To_String (Result (Algorithm)) /= "MD5"
            then
               Ada.Exceptions.Raise_Exception
                 (Constraint_Error'Identity,
                  "Only MD5 algorithm is supported.");
            end if;

            --  The parameter Stale is true when the Digest value is correct
            --  but the nonce value is too old or incorrect.
            --
            --  This mean that an interactive HTTP client should not ask
            --  name/password from the user, and try to use name/password from
            --  the previous successful authentication attempt.
            --  We do not need to check Stale authentication parameter
            --  for now, because our client is not interactive, so we are not
            --  going to ask user to input the name/password anyway. We could
            --  uncomment it later, when we would provide some interactive
            --  behavior to AWS.Client or interface to the interactive
            --  programs by callback to the AWS.Client.
            --
            --  if Messages.Match ("true", To_String (Result (Stale))) then
            --     null;
            --  end if;
         end if;
      end Parse_Authenticate_Line;

      --------------------
      -- Set_Keep_Alive --
      --------------------

      procedure Set_Keep_Alive (Data : in String) is
      begin
         if Messages.Match (Data, "Close") then
            Keep_Alive := False;

         elsif Messages.Match (Data, "Keep-Alive") then
            Keep_Alive := True;
         end if;
      end Set_Keep_Alive;

   begin
      Content_Length := 0;

      for Level in Authentication_Level'Range loop
         Connection.Auth (Level).Requested := False;
      end loop;

      --  We should check the Messages.HTTP_Token only in the first line
      --  and raise an exception if the first line does not match HTTP_Token.
      declare
         Line : constant String := Net.Buffered.Get_Line (Sock);
      begin
         Debug_Message ("< ", Line);

         --  Checking the first line in the HTTP header.
         --  It must match Messages.HTTP_Token.

         if Messages.Match (Line, Messages.HTTP_Token) then
            Status := Messages.Status_Code'Value
              ('S' & Line (Messages.HTTP_Token'Last + 5
                             .. Messages.HTTP_Token'Last + 7));

            --  By default HTTP/1.0 connection is not keep-alive but
            --  HTTP/1.1 is keep-alive

            Keep_Alive
              := Line (Messages.HTTP_Token'Last + 1
                         .. Messages.HTTP_Token'Last + 3) >= "1.1";
         else
            --  or else it is wrong answer from server.
            raise Protocol_Error;
         end if;
      end;

      loop
         declare
            use type Messages.Status_Code;
            Line : constant String := Net.Buffered.Get_Line (Sock);
         begin
            Debug_Message ("< ", Line);

            if Line = End_Section and then Status /= Messages.S100 then
               --  Exit if we got and End_Section (empty line) and the status
               --  is not 100 (continue).
               exit;

            elsif Messages.Match (Line, Messages.Content_Type_Token) then
               Content_Type := To_Unbounded_String
                 (Line (Messages.Content_Type_Token'Last + 1
                          .. Line'Last));

            elsif Messages.Match (Line, Messages.Content_Length_Token) then
               Content_Length := Natural'Value
                 (Line (Messages.Content_Length_Range'Last + 1
                          .. Line'Last));

            elsif Messages.Match (Line, Messages.Location_Token) then
               Location := To_Unbounded_String
                 (Line (Messages.Location_Token'Last + 1 .. Line'Last));

            elsif Messages.Match
              (Line, Messages.Transfer_Encoding_Token)
            then

               Transfer_Encoding := To_Unbounded_String
                 (Line (Messages.Transfer_Encoding_Range'Last + 1
                          .. Line'Last));

            elsif Messages.Match (Line, Messages.Connection_Token) then
               Set_Keep_Alive
                 (Line (Messages.Connection_Token'Last + 1 .. Line'Last));

            elsif
              Messages.Match (Line, Messages.Proxy_Connection_Token)
            then
               Set_Keep_Alive
                 (Line
                    (Messages.Proxy_Connection_Token'Last + 1 .. Line'Last));

            elsif Messages.Match (Line, Messages.Set_Cookie_Token) then
               Connection.Cookie := To_Unbounded_String
                 (Line (Messages.Set_Cookie_Token'Last + 1 .. Line'Last));

            elsif Messages.Match
              (Line, Messages.WWW_Authenticate_Token)
            then
               Parse_Authenticate_Line
                 (WWW,
                  Line (Messages.WWW_Authenticate_Token'Last + 1
                          .. Line'Last));

            elsif Messages.Match
              (Line, Messages.Proxy_Authenticate_Token)
            then
               Parse_Authenticate_Line
                 (Proxy,
                  Line (Messages.Proxy_Authenticate_Token'Last + 1
                          .. Line'Last));

            else
               --  Everything else is ignored right now
               null;
            end if;
         end;
      end loop;
   end Parse_Header;

   ----------
   -- Post --
   ----------

   function Post
     (URL        : in String;
      Data       : in String;
      User       : in String          := No_Data;
      Pwd        : in String          := No_Data;
      Proxy      : in String          := No_Data;
      Proxy_User : in String          := No_Data;
      Proxy_Pwd  : in String          := No_Data;
      Timeouts   : in Timeouts_Values := No_Timeout)
      return Response.Data
   is
      use Streams;
   begin
      return Post (URL, Translator.To_Stream_Element_Array (Data),
                   User, Pwd, Proxy, Proxy_User, Proxy_Pwd, Timeouts);
   end Post;

   ----------
   -- Post --
   ----------

   function Post
     (URL        : in String;
      Data       : in Streams.Stream_Element_Array;
      User       : in String          := No_Data;
      Pwd        : in String          := No_Data;
      Proxy      : in String          := No_Data;
      Proxy_User : in String          := No_Data;
      Proxy_Pwd  : in String          := No_Data;
      Timeouts   : in Timeouts_Values := No_Timeout)
      return Response.Data
   is
      Connection : HTTP_Connection;
      Result     : Response.Data;

   begin
      Create (Connection,
              URL, User, Pwd, Proxy, Proxy_User, Proxy_Pwd,
              Persistent => False,
              Timeouts   => Timeouts);

      Post (Connection, Result, Data);
      Close (Connection);
      return Result;

   exception
      when others =>
         Close (Connection);
         raise;
   end Post;

   ----------
   -- Post --
   ----------

   procedure Post
     (Connection : in out HTTP_Connection;
      Result     :    out Response.Data;
      Data       : in     Streams.Stream_Element_Array;
      URI        : in     String := No_Data)
   is
      No_Data   : Unbounded_String renames Null_Unbounded_String;
      Try_Count : Natural := Connection.Retry;

      Auth_Attempts : Auth_Attempts_Count := (others => 2);
      Auth_Is_Over  : Boolean;

   begin

      loop
         begin
            Open_Send_Common_Header (Connection, "POST", URI);

            declare
               Sock : Net.Socket_Type'Class renames Connection.Socket.all;
            begin

               if Connection.SOAPAction = No_Data then
                  Send_Header
                    (Sock,
                     Messages.Content_Type (MIME.Appl_Form_Data));

               else
                  Send_Header
                    (Sock,
                     Messages.Content_Type (MIME.Text_XML));
               end if;

               --  Send message Content_Length

               Send_Header (Sock, Messages.Content_Length (Data'Length));

               Net.Buffered.New_Line (Sock);

               --  Send message body

               Net.Buffered.Write (Sock, Data);
            end;

            --  Get answer from server

            Get_Response (Connection, Result, not Connection.Server_Push);

            Decrement_Authentication_Attempt
              (Connection, Auth_Attempts, Auth_Is_Over);

            if Auth_Is_Over then
               return;
            end if;

         exception

            when Net.Socket_Error =>

               Disconnect (Connection);

               if Try_Count = 0 then
                  Result := Response.Build
                    (MIME.Text_HTML, "Post Timeout", Messages.S408);
                  Set_Phase (Connection, Not_Monitored);
                  exit;
               end if;

               Try_Count := Try_Count - 1;
         end;
      end loop;
   end Post;

   ----------
   -- Post --
   ----------

   procedure Post
     (Connection : in out HTTP_Connection;
      Result     :    out Response.Data;
      Data       : in     String;
      URI        : in     String := No_Data) is
   begin
      Post (Connection, Result,
            Translator.To_Stream_Element_Array (Data), URI);
   end Post;

   ---------
   -- Put --
   ---------

   function Put
     (URL        : in String;
      Data       : in String;
      User       : in String          := No_Data;
      Pwd        : in String          := No_Data;
      Proxy      : in String          := No_Data;
      Proxy_User : in String          := No_Data;
      Proxy_Pwd  : in String          := No_Data;
      Timeouts   : in Timeouts_Values := No_Timeout)
     return Response.Data
   is
      Connection : HTTP_Connection;
      Result     : Response.Data;

   begin
      Create (Connection,
              URL, User, Pwd, Proxy, Proxy_User, Proxy_Pwd,
              Persistent => False,
              Timeouts   => Timeouts);

      Put (Connection, Result, Data);
      Close (Connection);
      return Result;

   exception
      when others =>
         Close (Connection);
         raise;
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put
     (Connection : in out HTTP_Connection;
      Result     :    out Response.Data;
      Data       : in     String;
      URI        : in     String          := No_Data)
   is
      CT         : Unbounded_String;
      CT_Len     : Natural;
      TE         : Unbounded_String;
      Status     : Messages.Status_Code;
      Location   : Unbounded_String;
      Keep_Alive : Boolean;

      Try_Count  : Natural := Connection.Retry;

      Auth_Attempts : Auth_Attempts_Count := (others => 2);
      Auth_Is_Over  : Boolean;

   begin

      loop

         begin
            Open_Send_Common_Header (Connection, "PUT", URI);

            --  Send message Content_Length

            Send_Header
              (Connection.Socket.all, Messages.Content_Length (Data'Length));

            Net.Buffered.New_Line (Connection.Socket.all);

            --  Send message body

            Net.Buffered.Put_Line (Connection.Socket.all, Data);

            --  Get answer from server

            Parse_Header
              (Connection, Status, CT_Len, CT, TE, Location, Keep_Alive);

            if not Keep_Alive then
               Disconnect (Connection);
            end if;

            Decrement_Authentication_Attempt
              (Connection, Auth_Attempts, Auth_Is_Over);

            if Auth_Is_Over then
               Result := Response.Acknowledge (Status);
               return;
            end if;

         exception
            when Net.Socket_Error =>

               Disconnect (Connection);

               if Try_Count = 0 then
                  Result := Response.Build
                    (MIME.Text_HTML, "Put Timeout", Messages.S408);
                  Set_Phase (Connection, Not_Monitored);
                  exit;
               end if;

               Try_Count := Try_Count - 1;
         end;
      end loop;
   end Put;

   ----------------
   -- Read_Until --
   ----------------

   function Read_Until
     (Connection : in HTTP_Connection;
      Delimiter  : in String)
     return String
   is
      Result     : Unbounded_String;
   begin
      Read_Until (Connection.Self.all, Delimiter, Result);
      return To_String (Result);
   end Read_Until;

   procedure Read_Until
     (Connection : in out HTTP_Connection;
      Delimiter  : in     String;
      Result     : in out Ada.Strings.Unbounded.Unbounded_String)
   is
      Sample_Idx : Natural := Delimiter'First;
      Buffer     : String (1 .. 1024);

   begin
      Set_Phase (Connection, Receive);

      Main : loop
         for I in Buffer'Range loop
            begin
               Buffer (I) := Net.Buffered.Get_Char (Connection.Socket.all);
            exception
               when Net.Socket_Error =>
                  Append (Result, Buffer (Buffer'First .. I - 1));
                  exit Main;
            end;

            if Buffer (I) = Delimiter (Sample_Idx) then

               if Sample_Idx = Delimiter'Last then
                  Append (Result, Buffer (Buffer'First .. I));
                  exit Main;
               else
                  Sample_Idx := Sample_Idx + 1;
               end if;

            else
               Sample_Idx := Delimiter'First;
            end if;
         end loop;

         Append (Result, Buffer);
      end loop Main;

      Set_Phase (Connection, Not_Monitored);
   end Read_Until;

   -----------------
   -- Send_Header --
   -----------------

   procedure Send_Header
     (Sock : in Net.Socket_Type'Class;
      Data : in String) is
   begin
      Net.Buffered.Put_Line (Sock, Data);
      Debug_Message ("> ", Data);
   end Send_Header;

   ------------------------
   -- Set_Authentication --
   ------------------------

   procedure Set_Authentication
     (Auth       :    out Authentication_Type;
      User       : in     String;
      Pwd        : in     String;
      Mode       : in     Authentication_Mode)
   is
   begin
      Auth.User      := To_Unbounded_String (User);
      Auth.Pwd       := To_Unbounded_String (Pwd);
      Auth.Init_Mode := Mode;

      if Mode = Basic then
         Auth.Work_Mode := Basic;
      end if;
   end Set_Authentication;

   ---------------
   -- Set_Debug --
   ---------------

   procedure Set_Debug (On : in Boolean) is
   begin
      Debug_On := On;
   end Set_Debug;

   ---------------
   -- Set_Phase --
   ---------------

   procedure Set_Phase
     (Connection : in out HTTP_Connection;
      Phase      : in     Client_Phase) is
   begin
      Connection.Current_Phase := Phase;

      if Connection.Cleaner /= null then
         Connection.Cleaner.Next_Phase;
      end if;
   end Set_Phase;

   ------------------------------
   -- Set_Proxy_Authentication --
   ------------------------------

   procedure Set_Proxy_Authentication
     (Connection : in out HTTP_Connection;
      User       : in     String;
      Pwd        : in     String;
      Mode       : in     Authentication_Mode)
   is
   begin
      Set_Authentication
        (Auth => Connection.Auth (Proxy),
         User => User,
         Pwd  => Pwd,
         Mode => Mode);
   end Set_Proxy_Authentication;

   ----------------------------
   -- Set_WWW_Authentication --
   ----------------------------

   procedure Set_WWW_Authentication
     (Connection : in out HTTP_Connection;
      User       : in     String;
      Pwd        : in     String;
      Mode       : in     Authentication_Mode)
   is
   begin
      Set_Authentication
        (Auth => Connection.Auth (WWW),
         User => User,
         Pwd  => Pwd,
         Mode => Mode);
   end Set_WWW_Authentication;

   ---------------
   -- SOAP_Post --
   ---------------

   function SOAP_Post
     (URL        : in String;
      Data       : in String;
      SOAPAction : in String;
      User       : in String          := No_Data;
      Pwd        : in String          := No_Data;
      Proxy      : in String          := No_Data;
      Proxy_User : in String          := No_Data;
      Proxy_Pwd  : in String          := No_Data;
      Timeouts   : in Timeouts_Values := No_Timeout)
      return Response.Data
   is
      Connection : HTTP_Connection;
      Result     : Response.Data;

   begin
      Create (Connection,
              URL, User, Pwd, Proxy, Proxy_User, Proxy_Pwd,
              SOAPAction => SOAPAction,
              Persistent => False,
              Timeouts   => Timeouts);

      Post (Connection, Result, Data);
      Close (Connection);
      return Result;
   end SOAP_Post;

   function SOAP_Post
     (Connection : access HTTP_Connection;
      Data       : in     String)
      return Response.Data
   is
      Result     : Response.Data;
   begin
      Post (Connection.all, Result, Data);
      return Result;
   end SOAP_Post;

   ------------
   -- Upload --
   ------------

   procedure Upload
     (Connection : in out HTTP_Connection;
      Result     :    out Response.Data;
      Filename   : in     String;
      URI        : in     String := No_Data)
   is
      Pref_Suf  : constant String        := "--";
      Now       : constant Calendar.Time := Calendar.Clock;
      Boundary  : constant String
        := "AWS_File_Upload-" & GNAT.Calendar.Time_IO.Image (Now, "%s");

      CT        : constant String
        := Messages.Content_Type (MIME.Content_Type (Filename));

      CD        : constant String
        := Messages.Content_Disposition ("form-data", "filename", Filename);

      Try_Count : Natural := Connection.Retry;

      Auth_Attempts : Auth_Attempts_Count := (others => 2);
      Auth_Is_Over  : Boolean;

      function Content_Length return Integer;
      --  Returns the total message content length.

      procedure Send_File;
      --  Send file content to the server.

      --------------------
      -- Content_Length --
      --------------------

      function Content_Length return Integer is
      begin
         return 2 * Boundary'Length                -- 2 boundaries
           + 2                                     -- second one end with "--"
           + 10                                    -- 5 lines with CR+LF
           + CT'Length                             -- content length header
           + CD'Length                             -- content disposition head
           + Integer (OS_Lib.File_Size (Filename)) -- file size
           + 2;                                    -- CR+LF after file data
      end Content_Length;

      ---------------
      -- Send_File --
      ---------------

      procedure Send_File is
         Sock     : Net.Socket_Type'Class renames Connection.Socket.all;
         Buffer   : Streams.Stream_Element_Array (1 .. 4_096);
         Last     : Streams.Stream_Element_Offset;
         File     : Streams.Stream_IO.File_Type;
      begin
         --  Send multipart message start boundary

         Net.Buffered.Put_Line (Sock, Pref_Suf & Boundary);

         --  Send Content-Disposition header

         Net.Buffered.Put_Line (Sock, CD);

         --  Send Content-Type: header

         Net.Buffered.Put_Line (Sock, CT);

         Net.Buffered.New_Line (Sock);

         --  Send file content

         Streams.Stream_IO.Open (File, Streams.Stream_IO.In_File, Filename);

         while not Streams.Stream_IO.End_Of_File (File) loop
            Streams.Stream_IO.Read (File, Buffer, Last);
            Net.Buffered.Write (Sock, Buffer (1 .. Last));
         end loop;

         Streams.Stream_IO.Close (File);

         Net.Buffered.New_Line (Sock);

         --  Send multipart message end boundary

         Net.Buffered.Put_Line (Sock, Pref_Suf & Boundary & Pref_Suf);
      end Send_File;

   begin
      loop
         begin
            Open_Send_Common_Header (Connection, "POST", URI);

            declare
               Sock : Net.Socket_Type'Class renames Connection.Socket.all;
            begin
               --  Send message Content-Type (Multipart/form-data)

               Send_Header
                 (Sock,
                  Messages.Content_Type (MIME.Multipart_Form_Data, Boundary));

               --  Send message Content-Length

               Send_Header (Sock, Messages.Content_Length (Content_Length));

               Net.Buffered.New_Line (Sock);

               --  Send message body

               Send_File;
            end;

            --  Get answer from server

            Get_Response (Connection, Result, not Connection.Server_Push);

            Decrement_Authentication_Attempt
              (Connection, Auth_Attempts, Auth_Is_Over);

            if Auth_Is_Over then
               return;
            end if;

         exception

            when Net.Socket_Error =>

               Disconnect (Connection);

               if Try_Count = 0 then
                  Result := Response.Build
                    (MIME.Text_HTML, "Post Timeout", Messages.S408);
                  Set_Phase (Connection, Not_Monitored);
                  exit;
               end if;

               Try_Count := Try_Count - 1;
         end;
      end loop;
   end Upload;

   function Upload
     (URL        : in String;
      Filename   : in String;
      User       : in String          := No_Data;
      Pwd        : in String          := No_Data;
      Proxy      : in String          := No_Data;
      Proxy_User : in String          := No_Data;
      Proxy_Pwd  : in String          := No_Data;
      Timeouts   : in Timeouts_Values := No_Timeout)
      return Response.Data
   is
      Connection : HTTP_Connection;
      Result     : Response.Data;

   begin
      Create (Connection,
              URL, User, Pwd, Proxy, Proxy_User, Proxy_Pwd,
              Persistent => False,
              Timeouts   => Timeouts);

      Upload (Connection, Result, Filename);

      Close (Connection);
      return Result;

   exception
      when others =>
         Close (Connection);
         raise;
   end Upload;

end AWS.Client;
