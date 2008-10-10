------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2008, AdaCore                     --
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

with Ada.Strings.Fixed;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;

with AWS.Client.HTTP_Utils;
with AWS.Headers.Set;
with AWS.Messages;
with AWS.MIME;
with AWS.Net.Buffered;
with AWS.Net.SSL;
with AWS.Translator;

package body AWS.Client is

   use Ada;
   use AWS.Client.HTTP_Utils;

   -----------
   -- Close --
   -----------

   procedure Close (Connection : in out HTTP_Connection) is
   begin
      Disconnect (Connection);

      if URL.Security (Connection.Host_URL) then
         Net.SSL.Release (Connection.SSL_Config);
      end if;

      if ZLib.Is_Open (Connection.Decode_Filter) then
         ZLib.Close (Connection.Decode_Filter, Ignore_Error => True);
      end if;

      Utils.Free (Connection.Decode_Buffer);
   end Close;

   ---------------------
   -- Connect_Timeout --
   ---------------------

   function Connect_Timeout (T : in Timeouts_Values) return Duration is
   begin
      return T.Connect;
   end Connect_Timeout;

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
      Persistent  : in     Boolean         := True;
      Timeouts    : in     Timeouts_Values := No_Timeout;
      Server_Push : in     Boolean         := False;
      Certificate : in     String          := Default.Client_Certificate;
      User_Agent  : in     String          := Default.User_Agent)
   is
      Host_URL    : constant AWS.URL.Object := AWS.URL.Parse (Host);
      Proxy_URL   : constant AWS.URL.Object := AWS.URL.Parse (Proxy);
      Connect_URL : AWS.URL.Object;
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
      Connection.Auth (WWW).User          := Value (User);
      Connection.Auth (WWW).Pwd           := Value (Pwd);
      Connection.Proxy                    := Value (Proxy);
      Connection.Proxy_URL                := Proxy_URL;
      Connection.Auth (Client.Proxy).User := Value (Proxy_User);
      Connection.Auth (Client.Proxy).Pwd  := Value (Proxy_Pwd);
      Connection.Retry                    := Create.Retry;
      Connection.Cookie                   := Null_Unbounded_String;
      Connection.Persistent               := Persistent;
      Connection.Streaming                := Server_Push;
      Connection.Certificate              := To_Unbounded_String (Certificate);
      Connection.Timeouts                 := Timeouts;

      Connection.User_Agent := To_Unbounded_String (User_Agent);

      --  If we have set the proxy or standard authentication we must set the
      --  authentication mode to Basic.

      if Proxy_User /= No_Data then
         Connection.Auth (Client.Proxy).Work_Mode := Basic;
      end if;

      if User /= No_Data then
         Connection.Auth (WWW).Work_Mode := Basic;
      end if;

      if URL.Security (Host_URL) then
         --  This is a secure connection, initialize the SSL layer

         Net.SSL.Initialize
           (Connection.SSL_Config, Certificate, Net.SSL.SSLv23_Client);
      end if;

      if Persistent and then Connection.Retry = 0 then
         --  In this case the connection termination can be initiated by the
         --  server or the client after a period. So the connection could be
         --  closed while trying to get some data from the server. To be nicer
         --  from user's point of view just make sure we retry at least one
         --  time before reporting an error.
         Connection.Retry := 1;
      end if;
   end Create;

   ---------------------
   -- Debug_Exception --
   ---------------------

   procedure Debug_Exception (E : in Ada.Exceptions.Exception_Occurrence) is
   begin
      Debug_Message ("! ", Ada.Exceptions.Exception_Message (E));
   end Debug_Exception;

   -------------------
   -- Debug_Message --
   -------------------

   procedure Debug_Message (Prefix, Message : in String) is
   begin
      if Debug_On then
         Text_IO.Put_Line (Prefix & Message);
      end if;
   end Debug_Message;

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
      Data_Range         : in Content_Range   := No_Range;
      Follow_Redirection : in Boolean         := False;
      Certificate        : in String          := Default.Client_Certificate)
      return Response.Data
   is
      use type Messages.Status_Code;

      Result : Response.Data;
   begin
      declare
         Connection : HTTP_Connection;
      begin
         Create (Connection,
                 URL, User, Pwd, Proxy, Proxy_User, Proxy_Pwd,
                 Persistent  => False,
                 Certificate => Certificate,
                 Timeouts    => Timeouts);

         Get (Connection, Result, Data_Range => Data_Range);

         Close (Connection);
      exception
         when others =>
            Close (Connection);
            raise;
      end;

      if Follow_Redirection
        and then Response.Status_Code (Result) = Messages.S305
      then
         --  This is "Use Proxy" message, Location point to the proxy to use.
         --  We do not have the login/password for the proxy.
         return Get
           (URL, User, Pwd, Response.Location (Result),
            Timeouts => Timeouts, Follow_Redirection => Follow_Redirection,
            Certificate => Certificate);

      elsif Follow_Redirection
          and then
        Response.Status_Code (Result) in Messages.S301 .. Messages.S307
          and then
        Response.Status_Code (Result) /= Messages.S304
      then
         --  All other redirections, 304 is not one of them
         return Get
           (Response.Location (Result), User, Pwd,
            Proxy, Proxy_User, Proxy_Pwd, Timeouts,
            Data_Range, Follow_Redirection, Certificate => Certificate);
      else
         return Result;
      end if;
   end Get;

   ---------
   -- Get --
   ---------

   procedure Get
     (Connection : in out HTTP_Connection;
      Result     :    out Response.Data;
      URI        : in     String          := No_Data;
      Data_Range : in     Content_Range   := No_Range)
   is
      use Ada.Real_Time;
      Stamp         : constant Time := Clock;

      Try_Count     : Natural := Connection.Retry;

      Auth_Attempts : Auth_Attempts_Count := (others => 2);
      Auth_Is_Over  : Boolean;
   begin
      Connection.Data_Range := Data_Range;

      Retry : loop
         begin
            Open_Send_Common_Header (Connection, "GET", URI);

            Net.Buffered.New_Line (Connection.Socket.all);

            Get_Response (Connection, Result, not Connection.Streaming);

            Decrement_Authentication_Attempt
              (Connection, Auth_Attempts, Auth_Is_Over);

            if Auth_Is_Over then
               return;
            end if;

         exception
            when E : Net.Socket_Error | Connection_Error =>
               Debug_Exception (E);

               Disconnect (Connection);

               if Try_Count = 0
                 or else Clock - Stamp >= Connection.Timeouts.Response
               then
                  Result := Response.Build
                    (MIME.Text_HTML, "Get Timeout", Messages.S408);

                  exit Retry;
               end if;

               Try_Count := Try_Count - 1;
         end;
      end loop Retry;
   end Get;

   ---------------------
   -- Get_Certificate --
   ---------------------

   function Get_Certificate
     (Connection : in HTTP_Connection)
      return Net.SSL.Certificate.Object
   is
      use type Net.Socket_Access;
   begin
      if not Connection.Opened then
         --  SSL socket have to be created to get certificate
         Connect (Connection.Self.all);
      end if;

      if Connection.Socket.all in AWS.Net.SSL.Socket_Type'Class then
         return Net.SSL.Certificate.Get
           (Net.SSL.Socket_Type (Connection.Socket.all));
      else
         return Net.SSL.Certificate.Undefined;
      end if;
   end Get_Certificate;

   ----------------
   -- Get_Cookie --
   ----------------

   function Get_Cookie (Connection : in HTTP_Connection) return String is
   begin
      return To_String (Connection.Cookie);
   end Get_Cookie;

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
      Timeouts   : in Timeouts_Values := No_Timeout) return Response.Data
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
      use Ada.Real_Time;
      Stamp         : constant Time := Clock;
      Try_Count     : Natural := Connection.Retry;
      Auth_Attempts : Auth_Attempts_Count := (others => 2);
      Auth_Is_Over  : Boolean;
   begin
      Retry : loop
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
            when E : Net.Socket_Error =>
               Debug_Exception (E);

               Disconnect (Connection);

               if Try_Count = 0
                 or else Clock - Stamp >= Connection.Timeouts.Response
               then
                  Result := Response.Build
                    (MIME.Text_HTML, "Head Timeout", Messages.S408);
                  exit Retry;
               end if;

               Try_Count := Try_Count - 1;
         end;
      end loop Retry;
   end Head;

   ----------
   -- Post --
   ----------

   function Post
     (URL          : in String;
      Data         : in String;
      Content_Type : in String               := No_Data;
      User         : in String               := No_Data;
      Pwd          : in String               := No_Data;
      Proxy        : in String               := No_Data;
      Proxy_User   : in String               := No_Data;
      Proxy_Pwd    : in String               := No_Data;
      Timeouts     : in Timeouts_Values      := No_Timeout;
      Attachments  : in AWS.Attachments.List := AWS.Attachments.Empty_List)
      return Response.Data
   is
      Connection : HTTP_Connection;
      Result     : Response.Data;
   begin
      Create (Connection,
              URL, User, Pwd, Proxy, Proxy_User, Proxy_Pwd,
              Persistent => False,
              Timeouts   => Timeouts);

      Post (Connection, Result, Data,
            Content_Type, Attachments => Attachments);

      Close (Connection);
      return Result;
   exception
      when others =>
         Close (Connection);
         raise;
   end Post;

   function Post
     (URL          : in String;
      Data         : in Streams.Stream_Element_Array;
      Content_Type : in String               := No_Data;
      User         : in String               := No_Data;
      Pwd          : in String               := No_Data;
      Proxy        : in String               := No_Data;
      Proxy_User   : in String               := No_Data;
      Proxy_Pwd    : in String               := No_Data;
      Timeouts     : in Timeouts_Values      := No_Timeout;
      Attachments  : in AWS.Attachments.List := AWS.Attachments.Empty_List)
      return Response.Data
   is
      Connection : HTTP_Connection;
      Result     : Response.Data;
   begin
      Create (Connection,
              URL, User, Pwd, Proxy, Proxy_User, Proxy_Pwd,
              Persistent => False,
              Timeouts   => Timeouts);

      Post (Connection, Result, Data,
            Content_Type, Attachments => Attachments);

      Close (Connection);
      return Result;
   exception
      when others =>
         Close (Connection);
         raise;
   end Post;

   procedure Post
     (Connection   : in out HTTP_Connection;
      Result       :    out Response.Data;
      Data         : in     Streams.Stream_Element_Array;
      Content_Type : in     String               := No_Data;
      URI          : in     String               := No_Data;
      Attachments  : in     AWS.Attachments.List := AWS.Attachments.Empty_List)
   is
   begin
      if Content_Type = No_Data then
         Internal_Post
           (Connection,
            Result,
            Data,
            URI,
            SOAPAction   => No_Data,
            Content_Type => MIME.Application_Octet_Stream,
            Attachments  => Attachments);
      else
         Internal_Post
           (Connection,
            Result,
            Data,
            URI,
            SOAPAction   => No_Data,
            Content_Type => Content_Type,
            Attachments  => Attachments);
      end if;
   end Post;

   procedure Post
     (Connection   : in out HTTP_Connection;
      Result       :    out Response.Data;
      Data         : in     String;
      Content_Type : in     String               := No_Data;
      URI          : in     String               := No_Data;
      Attachments  : in     AWS.Attachments.List := AWS.Attachments.Empty_List)
   is
   begin
      if Content_Type = No_Data then
         Internal_Post
           (Connection,
            Result,
            Translator.To_Stream_Element_Array (Data),
            URI,
            SOAPAction   => No_Data,
            Content_Type => MIME.Application_Form_Data,
            Attachments  => Attachments);
      else
         Internal_Post
           (Connection,
            Result,
            Translator.To_Stream_Element_Array (Data),
            URI,
            SOAPAction   => No_Data,
            Content_Type => Content_Type,
            Attachments  => Attachments);
      end if;
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
      Timeouts   : in Timeouts_Values := No_Timeout) return Response.Data
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
      use Ada.Real_Time;
      Stamp         : constant Time := Clock;
      Keep_Alive    : Boolean;
      Try_Count     : Natural := Connection.Retry;
      Auth_Attempts : Auth_Attempts_Count := (others => 2);
      Auth_Is_Over  : Boolean;
   begin
      Retry : loop

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
              (Connection, Result, Keep_Alive);

            if not Keep_Alive then
               Disconnect (Connection);
            end if;

            Decrement_Authentication_Attempt
              (Connection, Auth_Attempts, Auth_Is_Over);

            if Auth_Is_Over then
               return;
            end if;

         exception
            when E : Net.Socket_Error =>
               Debug_Exception (E);

               Disconnect (Connection);

               if Try_Count = 0
                 or else Clock - Stamp >= Connection.Timeouts.Response
               then
                  Result := Response.Build
                    (MIME.Text_HTML, "Put Timeout", Messages.S408);
                  exit Retry;
               end if;

               Try_Count := Try_Count - 1;
         end;
      end loop Retry;
   end Put;

   ----------
   -- Read --
   ----------

   procedure Read
     (Connection : in out HTTP_Connection;
      Data       :    out Ada.Streams.Stream_Element_Array;
      Last       :    out Ada.Streams.Stream_Element_Offset)
   is
      First : Stream_Element_Offset := Data'First;
   begin
      loop
         Read_Some (Connection, Data (First .. Data'Last), Last);
         exit when Last = Data'Last or else Last < First;
         First := Last + 1;
      end loop;
   end Read;

   ---------------
   -- Read_Some --
   ---------------

   procedure Read_Some
     (Connection : in out HTTP_Connection;
      Data       :    out Ada.Streams.Stream_Element_Array;
      Last       :    out Ada.Streams.Stream_Element_Offset)
   is
      procedure Read_Internal
        (Data : out Ada.Streams.Stream_Element_Array;
         Last : out Ada.Streams.Stream_Element_Offset);
      --  Read the encoded data as is from HTTP connection

      -------------------
      -- Read_Internal --
      -------------------

      procedure Read_Internal
        (Data : out Ada.Streams.Stream_Element_Array;
         Last : out Ada.Streams.Stream_Element_Offset)
      is
         Sock  : Net.Socket_Type'Class renames Connection.Socket.all;

         procedure Skip_Line;
         --  Skip a line in the sock stream

         procedure Read_Limited;
         --  Read Connection.Length characters if it can be contained in Data
         --  buffer otherwise just fill the remaining space in Data.

         ------------------
         -- Read_Limited --
         ------------------

         procedure Read_Limited is
            Limit : constant Stream_Element_Offset
              := Stream_Element_Offset'Min
                (Data'Last,
                 Data'First + Stream_Element_Offset (Connection.Length) - 1);
         begin
            Net.Buffered.Read (Sock, Data (Data'First .. Limit), Last);

            Connection.Length
              := Connection.Length - Integer (Last - Data'First + 1);
         end Read_Limited;

         ---------------
         -- Skip_Line --
         ---------------

         procedure Skip_Line is
            D : constant String := Net.Buffered.Get_Line (Sock);
            pragma Warnings (Off, D);
         begin
            null;
         end Skip_Line;

      begin
         case Connection.Transfer is
            when End_Response =>
               Last := Data'First - 1;

            when Until_Close  =>
               begin
                  Net.Buffered.Read (Sock, Data, Last);
               exception
                  when E : Net.Socket_Error =>
                     Debug_Exception (E);
                     Connection.Transfer := End_Response;
                     Last := Data'First - 1;
               end;

            when Content_Length =>
               if Connection.Length = 0 then
                  Connection.Transfer := End_Response;
                  Last := Data'First - 1;
                  return;
               end if;

               Read_Limited;

            when Chunked =>
               if Connection.Length = 0 then
                  Connection.Length
                    := Utils.Hex_Value
                         (Strings.Fixed.Trim
                            (Net.Buffered.Get_Line (Sock), Strings.Both));

                  if Connection.Length = 0 then
                     Skip_Line;
                     Connection.Transfer := End_Response;

                     Last := Data'First - 1;
                     return;
                  end if;
               end if;

               Read_Limited;

               if Connection.Length = 0 then
                  Skip_Line;
               end if;

            when None =>
               raise Constraint_Error;
         end case;
      end Read_Internal;

   begin
      if ZLib.Is_Open (Connection.Decode_Filter) then
         declare
            procedure Read is new ZLib.Read
              (Read_Internal, Connection.Decode_Buffer.all,
               Connection.Decode_First, Connection.Decode_Last,
               Allow_Read_Some => True);
            --  Decompress gzip or deflate encoded data
         begin
            Read (Connection.Decode_Filter, Data, Last);
         end;

         if Last < Data'First and Connection.Transfer = Chunked then
            --  When the 4 byte check sum is in the last chunk
            --  external routines could think that data is over,
            --  and would not call Read_Some any more. We have to
            --  read the last chunk of a chunked stream.

            Read_Internal (Data, Last);

            if Data'First <= Last or Connection.Transfer /= End_Response then
               raise Protocol_Error;
            end if;
         end if;

      else
         Read_Internal (Data, Last);
      end if;
   end Read_Some;

   ----------------
   -- Read_Until --
   ----------------

   function Read_Until
     (Connection : in HTTP_Connection;
      Delimiter  : in String;
      Wait       : in Boolean := True) return String is
   begin
      Net.Set_Timeout (Connection.Socket.all, Connection.Timeouts.Receive);

      return Translator.To_String
               (Net.Buffered.Read_Until
                  (Connection.Socket.all,
                   Translator.To_Stream_Element_Array (Delimiter),
                   Wait));
   end Read_Until;

   procedure Read_Until
     (Connection : in out HTTP_Connection;
      Delimiter  : in     String;
      Result     : in out Ada.Strings.Unbounded.Unbounded_String;
      Wait       : in     Boolean := True) is
   begin
      Result :=  Ada.Strings.Unbounded.To_Unbounded_String
                   (Read_Until (Connection, Delimiter, Wait));
   end Read_Until;

   ---------------------
   -- Receive_Timeout --
   ---------------------

   function Receive_Timeout (T : in Timeouts_Values) return Duration is
   begin
      return T.Receive;
   end Receive_Timeout;

   ----------------------
   -- Response_Timeout --
   ----------------------

   function Response_Timeout (T : in Timeouts_Values) return Duration is
   begin
      return Ada.Real_Time.To_Duration (T.Response);
   end Response_Timeout;

   ------------------
   -- Send_Timeout --
   ------------------

   function Send_Timeout (T : in Timeouts_Values) return Duration is
   begin
      return T.Send;
   end Send_Timeout;

   ----------------
   -- Set_Cookie --
   ----------------

   procedure Set_Cookie
     (Connection : in out HTTP_Connection; Cookie : in String) is
   begin
      Connection.Cookie := To_Unbounded_String (Cookie);
   end Set_Cookie;

   ---------------
   -- Set_Debug --
   ---------------

   procedure Set_Debug (On : in Boolean) is
   begin
      Debug_On := On;
      AWS.Headers.Set.Debug (On);
   end Set_Debug;

   ------------------------------
   -- Set_Proxy_Authentication --
   ------------------------------

   procedure Set_Proxy_Authentication
     (Connection : in out HTTP_Connection;
      User       : in     String;
      Pwd        : in     String;
      Mode       : in     Authentication_Mode) is
   begin
      Set_Authentication
        (Auth => Connection.Auth (Proxy),
         User => User,
         Pwd  => Pwd,
         Mode => Mode);
   end Set_Proxy_Authentication;

   --------------------------
   -- Set_Streaming_Output --
   --------------------------

   procedure Set_Streaming_Output
     (Connection : in out HTTP_Connection;
      Value      : in     Boolean) is
   begin
      Connection.Streaming := Value;
   end Set_Streaming_Output;

   ----------------------------
   -- Set_WWW_Authentication --
   ----------------------------

   procedure Set_WWW_Authentication
     (Connection : in out HTTP_Connection;
      User       : in     String;
      Pwd        : in     String;
      Mode       : in     Authentication_Mode) is
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
     (URL         : in String;
      Data        : in String;
      SOAPAction  : in String;
      User        : in String               := No_Data;
      Pwd         : in String               := No_Data;
      Proxy       : in String               := No_Data;
      Proxy_User  : in String               := No_Data;
      Proxy_Pwd   : in String               := No_Data;
      Timeouts    : in Timeouts_Values      := No_Timeout;
      Attachments : in AWS.Attachments.List := AWS.Attachments.Empty_List)
      return Response.Data
   is
      Connection : HTTP_Connection;
      Result     : Response.Data;
   begin
      Create (Connection,
              URL, User, Pwd, Proxy, Proxy_User, Proxy_Pwd,
              Persistent => False,
              Timeouts   => Timeouts);

      SOAP_Post (Connection, Result, SOAPAction,
                 Data        => Data,
                 Attachments => Attachments);

      Close (Connection);
      return Result;

   exception
      when others =>
         Close (Connection);
         raise;
   end SOAP_Post;

   procedure SOAP_Post
     (Connection  : in     HTTP_Connection;
      Result      :    out Response.Data;
      SOAPAction  : in     String;
      Data        : in     String;
      Streaming   : in     Boolean              := False;
      Attachments : in     AWS.Attachments.List := AWS.Attachments.Empty_List)
   is
      Save_Streaming : constant Boolean := Connection.Streaming;
   begin
      Connection.Self.Streaming := Streaming;

      Internal_Post
        (Connection   => Connection.Self.all,
         Result       => Result,
         Data         => AWS.Translator.To_Stream_Element_Array (Data),
         URI          => No_Data,
         SOAPAction   => SOAPAction,
         Content_Type => MIME.Text_XML,
         Attachments  => Attachments);

      Connection.Self.Streaming := Save_Streaming;
   end SOAP_Post;

   --------------
   -- Timeouts --
   --------------

   function Timeouts
     (Connect  : in Duration := Net.Forever;
      Send     : in Duration := Net.Forever;
      Receive  : in Duration := Net.Forever;
      Response : in Duration := Net.Forever) return Timeouts_Values is
   begin
      return (Connect  => Connect,
              Send     => Send,
              Receive  => Receive,
              Response => Ada.Real_Time.To_Time_Span (Response));
   end Timeouts;

   function Timeouts (Each : in Duration) return Timeouts_Values is
   begin
      return (Response => Ada.Real_Time.To_Time_Span (Each), others => Each);
   end Timeouts;

   ------------
   -- Upload --
   ------------

   procedure Upload
     (Connection : in out HTTP_Connection;
      Result     :    out Response.Data;
      Filename   : in     String;
      URI        : in     String := No_Data)
   is
      use Ada.Real_Time;
      Stamp    : constant Time   := Clock;
      Pref_Suf : constant String := "--";
      Boundary : constant String :=
        "AWS_File_Upload-" & Utils.Random_String (8);

      CT        : constant String
        := Messages.Content_Type (MIME.Content_Type (Filename));

      CD        : constant String
        := Messages.Content_Disposition ("form-data", "filename", Filename);

      Try_Count     : Natural := Connection.Retry;
      Auth_Attempts : Auth_Attempts_Count := (others => 2);
      Auth_Is_Over  : Boolean;

      function Content_Length return Integer;
      --  Returns the total message content length

      procedure Send_File;
      --  Send file content to the server

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
           + Integer (Utils.File_Size (Filename))  -- file size
           + 2;                                    -- CR+LF after file data
      end Content_Length;

      ---------------
      -- Send_File --
      ---------------

      procedure Send_File is
         Sock   : Net.Socket_Type'Class renames Connection.Socket.all;
         Buffer : Streams.Stream_Element_Array (1 .. 4_096);
         Last   : Streams.Stream_Element_Offset;
         File   : Streams.Stream_IO.File_Type;
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

      exception
         when Net.Socket_Error =>
            --  Properly close the file if needed
            if Streams.Stream_IO.Is_Open (File) then
               Streams.Stream_IO.Close (File);
            end if;
            raise;
      end Send_File;

   begin
      Retry : loop
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

            Get_Response (Connection, Result, not Connection.Streaming);

            Decrement_Authentication_Attempt
              (Connection, Auth_Attempts, Auth_Is_Over);

            if Auth_Is_Over then
               return;
            end if;

         exception

            when E : Net.Socket_Error | Connection_Error =>
               Debug_Exception (E);

               Disconnect (Connection);

               if Try_Count = 0
                 or else Clock - Stamp >= Connection.Timeouts.Response
               then
                  Result := Response.Build
                    (MIME.Text_HTML, "Upload Timeout", Messages.S408);
                  exit Retry;
               end if;

               Try_Count := Try_Count - 1;
         end;
      end loop Retry;
   end Upload;

   function Upload
     (URL        : in String;
      Filename   : in String;
      User       : in String          := No_Data;
      Pwd        : in String          := No_Data;
      Proxy      : in String          := No_Data;
      Proxy_User : in String          := No_Data;
      Proxy_Pwd  : in String          := No_Data;
      Timeouts   : in Timeouts_Values := No_Timeout) return Response.Data
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
