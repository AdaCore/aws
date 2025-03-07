------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2024, AdaCore                     --
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

with Ada.Strings.Fixed;
with Ada.Text_IO;

with AWS.Client.HTTP_Utils;
with AWS.Messages;
with AWS.MIME;
with AWS.Net.Buffered;
with AWS.Net.SSL;
with AWS.Translator;
with AWS.URL.Set;

package body AWS.Client is

   use AWS.Client.HTTP_Utils;

   ------------------------
   -- Cipher_Description --
   ------------------------

   function Cipher_Description (Connection : HTTP_Connection) return String is
      use type AWS.Net.Socket_Access;
   begin
      if Connection.Socket = null then
         return "";
      end if;

      return Connection.Socket.Cipher_Description;
   end Cipher_Description;

   -----------------------
   -- Clear_SSL_Session --
   -----------------------

   procedure Clear_SSL_Session (Connection : in out HTTP_Connection) is
   begin
      Net.SSL.Free (Connection.SSL_Session);
   end Clear_SSL_Session;

   -----------
   -- Close --
   -----------

   procedure Close (Connection : in out HTTP_Connection) is
   begin
      Disconnect (Connection);

      if Connection.Default_SSL_Config then
         Net.SSL.Release (Connection.SSL_Config);
      end if;

      if ZLib.Is_Open (Connection.Decode_Filter) then
         ZLib.Close (Connection.Decode_Filter, Ignore_Error => True);
      end if;

      Utils.Unchecked_Free (Connection.Decode_Buffer);

      Net.SSL.Free (Connection.SSL_Session);
   end Close;

   ---------------------
   -- Connect_Timeout --
   ---------------------

   function Connect_Timeout (T : Timeouts_Values) return Duration is
   begin
      return T.Connect;
   end Connect_Timeout;

   -----------------
   -- Copy_Cookie --
   -----------------

   procedure Copy_Cookie
     (Source      : HTTP_Connection;
      Destination : in out HTTP_Connection) is
   begin
      Destination.Cookie := Source.Cookie;
   end Copy_Cookie;

   ------------
   -- Create --
   ------------

   function Create
     (Host         : String;
      User         : String          := No_Data;
      Pwd          : String          := No_Data;
      Proxy        : String          := No_Data;
      Proxy_User   : String          := No_Data;
      Proxy_Pwd    : String          := No_Data;
      Retry        : Natural         := Retry_Default;
      Persistent   : Boolean         := True;
      Timeouts     : Timeouts_Values := No_Timeout;
      Server_Push  : Boolean         := False;
      User_Agent   : String          := Default.User_Agent;
      HTTP_Version : HTTP_Protocol   := HTTP_Default)
      return HTTP_Connection is
   begin
      return Connection : HTTP_Connection do
         Create (Connection, Host,
                 User, Pwd,
                 Proxy, Proxy_User, Proxy_Pwd,
                 Retry, Persistent, Timeouts,
                 Server_Push,
                 Net.SSL.Null_Config, User_Agent, HTTP_Version);
      end return;
   end Create;

   procedure Create
     (Connection   : in out HTTP_Connection;
      Host         : String;
      User         : String          := No_Data;
      Pwd          : String          := No_Data;
      Proxy        : String          := No_Data;
      Proxy_User   : String          := No_Data;
      Proxy_Pwd    : String          := No_Data;
      Retry        : Natural         := Retry_Default;
      Persistent   : Boolean         := True;
      Timeouts     : Timeouts_Values := No_Timeout;
      Server_Push  : Boolean         := False;
      SSL_Config   : Net.SSL.Config  := Net.SSL.Null_Config;
      User_Agent   : String          := Default.User_Agent;
      HTTP_Version : HTTP_Protocol   := HTTP_Default)
   is
      use type Net.SSL.Config;

      Host_URL    : constant AWS.URL.Object := AWS.URL.Parse (Host);
      Proxy_URL   : constant AWS.URL.Object := AWS.URL.Parse (Proxy);
      Connect_URL : AWS.URL.Object;
   begin
      --  If there is a proxy, the host to connect to is the proxy otherwise
      --  we connect to the Web server.

      if URL.Host (Host_URL) = "" then
         raise URL.URL_Error with "wrong host specified or missing protocol.";
      end if;

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
      Connection.Persistent               := Persistent;
      Connection.Streaming                := Server_Push;
      Connection.Timeouts                 := Timeouts;
      Connection.Config                   := AWS.Config.Get_Current;
      Connection.H2_Preface_Sent          := False;
      Connection.H2_Stream_Id             := 1;

      --  Server push is not supported in HTTPv2

      Connection.HTTP_Version := (if Server_Push
                                  then HTTPv1
                                  else HTTP_Version);

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

         Connection.Default_SSL_Config := SSL_Config = Net.SSL.Null_Config;

         if Connection.Default_SSL_Config then
            Connection.SSL_Config := Net.SSL.Get_Default_Client_Config;
         else
            Connection.SSL_Config := SSL_Config;
         end if;

         if Connection.HTTP_Version = HTTPv2 then
            Net.SSL.ALPN_Include (Connection.SSL_Config, Messages.H2_Token);
         end if;
      end if;

      if Persistent and then Connection.Retry = 0 then
         --  In this case the connection termination can be initiated by the
         --  server or the client after a period. So the connection could be
         --  closed while trying to get some data from the server. To be nicer
         --  from user's point of view just make sure we retry at least one
         --  time before reporting an error.
         Connection.Retry := 1;
      end if;

      if Connection.HTTP_Version = HTTPv2 then
         Connection.F_Headers.Names_Lowercased (True);
      end if;
   end Create;

   ---------------------
   -- Debug_Exception --
   ---------------------

   procedure Debug_Exception (E : Ada.Exceptions.Exception_Occurrence) is
   begin
      Debug_Message ("! ", Ada.Exceptions.Exception_Message (E));
   end Debug_Exception;

   -------------------
   -- Debug_Message --
   -------------------

   procedure Debug_Message (Prefix, Message : String) is
   begin
      if Debug_On then
         Text_IO.Put_Line (Prefix & Message);
      end if;
   end Debug_Message;

   ------------
   -- Delete --
   ------------

   function Delete
     (URL          : String;
      Data         : String;
      User         : String          := No_Data;
      Pwd          : String          := No_Data;
      Proxy        : String          := No_Data;
      Proxy_User   : String          := No_Data;
      Proxy_Pwd    : String          := No_Data;
      Timeouts     : Timeouts_Values := No_Timeout;
      Headers      : Header_List     := Empty_Header_List;
      User_Agent   : String          := Default.User_Agent;
      HTTP_Version : HTTP_Protocol   := HTTP_Default) return Response.Data is
   begin
      return Delete
        (URL, Translator.To_Stream_Element_Array (Data),
         User, Pwd, Proxy, Proxy_User, Proxy_Pwd, Timeouts,
         Headers, User_Agent, HTTP_Version);
   end Delete;

   function Delete
     (URL          : String;
      Data         : Stream_Element_Array;
      User         : String          := No_Data;
      Pwd          : String          := No_Data;
      Proxy        : String          := No_Data;
      Proxy_User   : String          := No_Data;
      Proxy_Pwd    : String          := No_Data;
      Timeouts     : Timeouts_Values := No_Timeout;
      Headers      : Header_List     := Empty_Header_List;
      User_Agent   : String          := Default.User_Agent;
      HTTP_Version : HTTP_Protocol   := HTTP_Default) return Response.Data
   is
      Connection : HTTP_Connection;
      Result     : Response.Data;
   begin
      Create (Connection,
              URL, User, Pwd, Proxy, Proxy_User, Proxy_Pwd,
              Persistent   => False,
              Timeouts     => Timeouts,
              User_Agent   => User_Agent,
              HTTP_Version => HTTP_Version);

      Delete (Connection, Result, Data, Headers => Headers);
      Close (Connection);
      return Result;
   exception
      when others =>
         Close (Connection);
         raise;
   end Delete;

   procedure Delete
     (Connection : in out HTTP_Connection;
      Result     : out Response.Data;
      Data       : Stream_Element_Array;
      URI        : String      := No_Data;
      Headers    : Header_List := Empty_Header_List) is
   begin
      Connection.F_Headers.Reset;

      Send_Request
        (Connection, HTTP_Utils.DELETE, Result, URI, Data, Headers);
   end Delete;

   procedure Delete
     (Connection : in out HTTP_Connection;
      Result     : out Response.Data;
      Data       : String;
      URI        : String      := No_Data;
      Headers    : Header_List := Empty_Header_List) is
   begin
      Connection.F_Headers.Reset;

      Delete
        (Connection => Connection,
         Result     => Result,
         Data       => Translator.To_Stream_Element_Array (Data),
         URI        => URI,
         Headers    => Headers);
   end Delete;

   ----------------------
   -- Error_Processing --
   ----------------------

   procedure Error_Processing
     (Connection : in out HTTP_Connection;
      Try_Count  : in out Natural;
      Result     : out Response.Data;
      Context    : String;
      E          : Ada.Exceptions.Exception_Occurrence;
      Stamp      : Ada.Real_Time.Time)
   is
      use Real_Time;
      Message : constant String := Exception_Information (E);
   begin
      Debug_Exception (E);

      if Real_Time.Clock - Stamp >= Connection.Timeouts.Response
        or else Net.Is_Timeout (Connection.Socket.all, E)
      then
         Result := Response.Build
           (MIME.Text_Plain, Context & " Timeout", Messages.S408);

      elsif Try_Count = 0 then
         Result := Response.Build
                     (MIME.Text_Plain,
                      Context & " request error. " & Message,
                      Messages.S400);

      else
         Result := Response.Empty;

         Try_Count := @ - 1;
      end if;

      Disconnect (Connection);
   end Error_Processing;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Connection : in out HTTP_Connection) is
   begin
      Close (Connection);
   end Finalize;

   ---------
   -- Get --
   ---------

   function Get
     (URL                : String;
      User               : String          := No_Data;
      Pwd                : String          := No_Data;
      Proxy              : String          := No_Data;
      Proxy_User         : String          := No_Data;
      Proxy_Pwd          : String          := No_Data;
      Timeouts           : Timeouts_Values := No_Timeout;
      Data_Range         : Content_Range   := No_Range;
      Follow_Redirection : Boolean         := False;
      Headers            : Header_List     := Empty_Header_List;
      User_Agent         : String          := Default.User_Agent;
      HTTP_Version       : HTTP_Protocol   := HTTP_Default)
      return Response.Data
   is
      use type Messages.Status_Code;

      Result   : Response.Data;
      Host_URL : AWS.URL.Object;
   begin
      declare
         Connection : HTTP_Connection;
      begin
         Create (Connection,
                 URL, User, Pwd, Proxy, Proxy_User, Proxy_Pwd,
                 Persistent   => False,
                 Timeouts     => Timeouts,
                 User_Agent   => User_Agent,
                 HTTP_Version => HTTP_Version);

         Host_URL := Connection.Host_URL; -- For the redirection case

         Get (Connection, Result,
              Data_Range => Data_Range, Headers => Headers);

         Close (Connection);
      exception
         when others =>
            Close (Connection);
            raise;
      end;

      declare
         SC : constant Messages.Status_Code := Response.Status_Code (Result);
      begin
         if Follow_Redirection and then SC = Messages.S305 then
            --  This is "Use Proxy" message, Location point to the proxy to
            --  use. We do not have the login/password for the proxy.

            return Get
              (URL, User, Pwd, Response.Location (Result),
               Timeouts           => Timeouts,
               Follow_Redirection => Follow_Redirection,
               Headers            => Headers,
               User_Agent         => User_Agent);

         elsif Follow_Redirection
           and then SC in Messages.Redirection
           and then SC /= Messages.S300 -- multiple choices
           and then SC /= Messages.S304 -- not modified, no redirection
         then
            declare
               use AWS.URL;
               Location : AWS.URL.Object :=
                            AWS.URL.Parse (Response.Location (Result));
            begin
               if Host (Location) = "" then
                  Set.Connection_Data
                    (Location,
                     Host (Host_URL),
                     Port (Host_URL),
                     Security (Host_URL));
               end if;

               return Get
                 (AWS.URL.URL (Location), User, Pwd,
                  Proxy, Proxy_User, Proxy_Pwd, Timeouts,
                  Data_Range, Follow_Redirection,
                  Headers     => Headers,
                  User_Agent  => User_Agent);
            end;
         else
            return Result;
         end if;
      end;
   end Get;

   ---------
   -- Get --
   ---------

   procedure Get
     (Connection : in out HTTP_Connection;
      Result     : out Response.Data;
      URI        : String          := No_Data;
      Data_Range : Content_Range   := No_Range;
      Headers    : Header_List     := Empty_Header_List) is
   begin
      Connection.Data_Range := Data_Range;
      Connection.F_Headers.Reset;

      Send_Request
        (Connection, HTTP_Utils.GET, Result, URI, HTTP_Utils.No_Data, Headers);
   end Get;

   ---------------------
   -- Get_Certificate --
   ---------------------

   function Get_Certificate
     (Connection : HTTP_Connection) return Net.SSL.Certificate.Object is
   begin
      if not Connection.Opened then
         --  SSL socket have to be created to get certificate
         Connect (Connection.Self.all);
      end if;

      if Connection.Socket.Is_Secure then
         return Net.SSL.Certificate.Get
           (Net.SSL.Socket_Type (Connection.Socket.all));
      else
         return Net.SSL.Certificate.Undefined;
      end if;
   end Get_Certificate;

   ----------------
   -- Get_Cookie --
   ----------------

   function Get_Cookie (Connection : HTTP_Connection) return String is
   begin
      return To_String (Connection.Cookie);
   end Get_Cookie;

   ----------
   -- Head --
   ----------

   function Head
     (URL          : String;
      User         : String          := No_Data;
      Pwd          : String          := No_Data;
      Proxy        : String          := No_Data;
      Proxy_User   : String          := No_Data;
      Proxy_Pwd    : String          := No_Data;
      Timeouts     : Timeouts_Values := No_Timeout;
      Headers      : Header_List     := Empty_Header_List;
      User_Agent   : String          := Default.User_Agent;
      HTTP_Version : HTTP_Protocol   := HTTP_Default)
      return Response.Data
   is
      Connection : HTTP_Connection;
      Result     : Response.Data;
   begin
      Create (Connection,
              URL, User, Pwd, Proxy, Proxy_User, Proxy_Pwd,
              Persistent   => False,
              Timeouts     => Timeouts,
              User_Agent   => User_Agent,
              HTTP_Version => HTTP_Version);

      Head (Connection, Result, Headers => Headers);
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
      Result     : out Response.Data;
      URI        : String      := No_Data;
      Headers    : Header_List := Empty_Header_List) is
   begin
      Connection.F_Headers.Reset;

      Send_Request
        (Connection, HTTP_Utils.HEAD,
         Result, URI, HTTP_Utils.No_Data, Headers);
   end Head;

   ----------
   -- Host --
   ----------

   function Host (Connection : HTTP_Connection) return String is
   begin
      return To_String (Connection.Host);
   end Host;

   ----------
   -- Post --
   ----------

   function Post
     (URL          : String;
      Data         : String;
      Content_Type : String          := No_Data;
      User         : String          := No_Data;
      Pwd          : String          := No_Data;
      Proxy        : String          := No_Data;
      Proxy_User   : String          := No_Data;
      Proxy_Pwd    : String          := No_Data;
      Timeouts     : Timeouts_Values := No_Timeout;
      Attachments  : Attachment_List := Empty_Attachment_List;
      Headers      : Header_List     := Empty_Header_List;
      User_Agent   : String          := Default.User_Agent;
      HTTP_Version : HTTP_Protocol   := HTTP_Default)
      return Response.Data
   is
      Connection : HTTP_Connection;
      Result     : Response.Data;
   begin
      Create (Connection,
              URL, User, Pwd, Proxy, Proxy_User, Proxy_Pwd,
              Persistent   => False,
              Timeouts     => Timeouts,
              User_Agent   => User_Agent,
              HTTP_Version => HTTP_Version);

      Post (Connection, Result, Data, Content_Type,
            Attachments => Attachments,
            Headers     => Headers);

      Close (Connection);
      return Result;
   exception
      when others =>
         Close (Connection);
         raise;
   end Post;

   function Post
     (URL          : String;
      Data         : Stream_Element_Array;
      Content_Type : String          := No_Data;
      User         : String          := No_Data;
      Pwd          : String          := No_Data;
      Proxy        : String          := No_Data;
      Proxy_User   : String          := No_Data;
      Proxy_Pwd    : String          := No_Data;
      Timeouts     : Timeouts_Values := No_Timeout;
      Attachments  : Attachment_List := Empty_Attachment_List;
      Headers      : Header_List     := Empty_Header_List;
      User_Agent   : String          := Default.User_Agent;
      HTTP_Version : HTTP_Protocol   := HTTP_Default)
      return Response.Data
   is
      Connection : HTTP_Connection;
      Result     : Response.Data;
   begin
      Create (Connection,
              URL, User, Pwd, Proxy, Proxy_User, Proxy_Pwd,
              Persistent   => False,
              Timeouts     => Timeouts,
              User_Agent   => User_Agent,
              HTTP_Version => HTTP_Version);

      Post (Connection, Result, Data, Content_Type,
            Attachments => Attachments,
            Headers     => Headers);

      Close (Connection);

      return Result;
   exception
      when others =>
         Close (Connection);
         raise;
   end Post;

   procedure Post
     (Connection   : in out HTTP_Connection;
      Result       : out Response.Data;
      Data         : Stream_Element_Array;
      Content_Type : String          := No_Data;
      URI          : String          := No_Data;
      Attachments  : Attachment_List := Empty_Attachment_List;
      Headers      : Header_List     := Empty_Header_List) is
   begin
      Connection.F_Headers.Reset;

      Internal_Post
        (Connection,
         Result,
         Data,
         URI,
         SOAPAction   => No_Data,
         Content_Type => (if Content_Type = No_Data
                          then MIME.Application_Octet_Stream
                          else Content_Type),
         Attachments  => Attachments,
         Headers      => Headers);
   end Post;

   procedure Post
     (Connection   : in out HTTP_Connection;
      Result       : out Response.Data;
      Data         : String;
      Content_Type : String          := No_Data;
      URI          : String          := No_Data;
      Attachments  : Attachment_List := Empty_Attachment_List;
      Headers      : Header_List     := Empty_Header_List) is
   begin
      Connection.F_Headers.Reset;

      --  For message larger than 20kb the conversion to Stream_Element_Array
      --  may create a stack overflow.

      if Data'Length > 20 * 1024 then
         declare
            SEA : Utils.Stream_Element_Array_Access :=
                    Translator.To_Stream_Element_Array (Data);
         begin
            Internal_Post
              (Connection,
               Result,
               SEA.all,
               URI,
               SOAPAction   => No_Data,
               Content_Type => (if Content_Type = No_Data
                                then MIME.Application_Form_Data
                                else Content_Type),
               Attachments  => Attachments,
               Headers      => Headers);

            Utils.Unchecked_Free (SEA);
         exception
            when others =>
               Utils.Unchecked_Free (SEA);
               raise;
         end;

      else
         Internal_Post
           (Connection,
            Result,
            Translator.To_Stream_Element_Array (Data),
            URI,
            SOAPAction   => No_Data,
            Content_Type => (if Content_Type = No_Data
                             then MIME.Application_Form_Data
                             else Content_Type),
            Attachments  => Attachments,
            Headers      => Headers);
      end if;
   end Post;

   ---------
   -- Put --
   ---------

   function Put
     (URL          : String;
      Data         : String;
      User         : String          := No_Data;
      Pwd          : String          := No_Data;
      Proxy        : String          := No_Data;
      Proxy_User   : String          := No_Data;
      Proxy_Pwd    : String          := No_Data;
      Timeouts     : Timeouts_Values := No_Timeout;
      Headers      : Header_List     := Empty_Header_List;
      User_Agent   : String          := Default.User_Agent;
      HTTP_Version : HTTP_Protocol   := HTTP_Default) return Response.Data
   is
      Connection : HTTP_Connection;
      Result     : Response.Data;
   begin
      Create (Connection,
              URL, User, Pwd, Proxy, Proxy_User, Proxy_Pwd,
              Persistent   => False,
              Timeouts     => Timeouts,
              User_Agent   => User_Agent,
              HTTP_Version => HTTP_Version);

      Put (Connection, Result, Data, Headers => Headers);
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
      Result     : out Response.Data;
      Data       : Stream_Element_Array;
      URI        : String      := No_Data;
      Headers    : Header_List := Empty_Header_List) is
   begin
      Send_Request (Connection, HTTP_Utils.PUT, Result, URI, Data, Headers);
   end Put;

   procedure Put
     (Connection : in out HTTP_Connection;
      Result     : out Response.Data;
      Data       : String;
      URI        : String      := No_Data;
      Headers    : Header_List := Empty_Header_List) is
   begin
      Put
        (Connection => Connection,
         Result     => Result,
         Data       => Translator.To_Stream_Element_Array (Data),
         URI        => URI,
         Headers    => Headers);
   end Put;

   ----------
   -- Read --
   ----------

   procedure Read
     (Connection : in out HTTP_Connection;
      Data       : out Stream_Element_Array;
      Last       : out Stream_Element_Offset)
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
      Data       : out Stream_Element_Array;
      Last       : out Stream_Element_Offset)
   is
      procedure Read_Internal
        (Data : out Stream_Element_Array;
         Last : out Stream_Element_Offset);
      --  Read the encoded data as is from HTTP connection

      -------------------
      -- Read_Internal --
      -------------------

      procedure Read_Internal
        (Data : out Stream_Element_Array;
         Last : out Stream_Element_Offset)
      is
         Sock : Net.Socket_Type'Class renames Connection.Socket.all;

         procedure Skip_Line;
         --  Skip a line in the sock stream

         procedure Read_Limited;
         --  Read Connection.Length characters if it can be contained in Data
         --  buffer otherwise just fill the remaining space in Data.

         ------------------
         -- Read_Limited --
         ------------------

         procedure Read_Limited is
            Limit : constant Stream_Element_Offset :=
                      Stream_Element_Offset'Min
                        (Data'Last,
                         Data'First
                           + Stream_Element_Offset (Connection.Length) - 1);
         begin
            Net.Buffered.Read (Sock, Data (Data'First .. Limit), Last);

            Connection.Length := @ - (Last - Data'First + 1);
         end Read_Limited;

         ---------------
         -- Skip_Line --
         ---------------

         procedure Skip_Line is
            D : constant String := Net.Buffered.Get_Line (Sock)
                  with Warnings => Off;
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
                  Connection.Length :=
                    Response.Content_Length_Type
                      (Utils.Hex_Value
                        (Strings.Fixed.Trim
                          (Net.Buffered.Get_Line (Sock), Strings.Both)));

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

         if Last < Data'First and then Connection.Transfer = Chunked then
            --  When the 4 byte check sum is in the last chunk
            --  external routines could think that data is over,
            --  and would not call Read_Some any more. We have to
            --  read the last chunk of a chunked stream.

            Read_Internal (Data, Last);

            if Data'First <= Last
              or else Connection.Transfer /= End_Response
            then
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
     (Connection : HTTP_Connection;
      Delimiter  : String;
      Wait       : Boolean := True) return String is
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
      Delimiter  : String;
      Result     : in out Ada.Strings.Unbounded.Unbounded_String;
      Wait       : Boolean := True) is
   begin
      Result := To_Unbounded_String
                  (Read_Until (Connection, Delimiter, Wait));
   end Read_Until;

   ---------------------
   -- Receive_Timeout --
   ---------------------

   function Receive_Timeout (T : Timeouts_Values) return Duration is
   begin
      return T.Receive;
   end Receive_Timeout;

   ----------------------
   -- Response_Timeout --
   ----------------------

   function Response_Timeout (T : Timeouts_Values) return Duration is
   begin
      return Ada.Real_Time.To_Duration (T.Response);
   end Response_Timeout;

   ------------------
   -- Send_Timeout --
   ------------------

   function Send_Timeout (T : Timeouts_Values) return Duration is
   begin
      return T.Send;
   end Send_Timeout;

   ----------------
   -- Set_Cookie --
   ----------------

   procedure Set_Cookie
     (Connection : in out HTTP_Connection; Cookie : String) is
   begin
      Connection.Cookie := To_Unbounded_String (Cookie);
   end Set_Cookie;

   ---------------
   -- Set_Debug --
   ---------------

   procedure Set_Debug (On : Boolean) is
   begin
      Debug_On := On;
      AWS.Headers.Debug (On);
   end Set_Debug;

   -----------------
   -- Set_Headers --
   -----------------

   procedure Set_Headers
     (Connection : in out HTTP_Connection; Headers : Header_List) is
   begin
      Connection.C_Headers := Headers;
   end Set_Headers;

   --------------------
   -- Set_Persistent --
   --------------------

   procedure Set_Persistent
     (Connection : in out HTTP_Connection; Value : Boolean) is
   begin
      Connection.Persistent := Value;
   end Set_Persistent;

   ------------------------------
   -- Set_Proxy_Authentication --
   ------------------------------

   procedure Set_Proxy_Authentication
     (Connection : in out HTTP_Connection;
      User       : String;
      Pwd        : String;
      Mode       : Authentication_Mode) is
   begin
      Set_Authentication
        (Auth => Connection.Auth (Proxy),
         User => User,
         Pwd  => Pwd,
         Mode => Mode);
   end Set_Proxy_Authentication;

   ---------------
   -- Set_Retry --
   ---------------

   procedure Set_Retry
     (Connection : in out HTTP_Connection; Value : Natural) is
   begin
      Connection.Retry := Value;
   end Set_Retry;

   --------------------------
   -- Set_Streaming_Output --
   --------------------------

   procedure Set_Streaming_Output
     (Connection : in out HTTP_Connection;
      Value      : Boolean) is
   begin
      Connection.Streaming := Value;
   end Set_Streaming_Output;

   ----------------------------
   -- Set_WWW_Authentication --
   ----------------------------

   procedure Set_WWW_Authentication
     (Connection : in out HTTP_Connection;
      User       : String;
      Pwd        : String;
      Mode       : Authentication_Mode) is
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
     (URL          : String;
      Data         : String;
      SOAPAction   : String;
      User         : String          := No_Data;
      Pwd          : String          := No_Data;
      Proxy        : String          := No_Data;
      Proxy_User   : String          := No_Data;
      Proxy_Pwd    : String          := No_Data;
      Timeouts     : Timeouts_Values := No_Timeout;
      Attachments  : Attachment_List := Empty_Attachment_List;
      Headers      : Header_List     := Empty_Header_List;
      User_Agent   : String          := Default.User_Agent;
      HTTP_Version : HTTP_Protocol   := HTTP_Default) return Response.Data
   is
      Connection : HTTP_Connection;
      Result     : Response.Data;
   begin
      Create (Connection,
              URL, User, Pwd, Proxy, Proxy_User, Proxy_Pwd,
              Persistent   => False,
              Timeouts     => Timeouts,
              User_Agent   => User_Agent,
              HTTP_Version => HTTP_Version);

      SOAP_Post (Connection, Result, SOAPAction,
                 Data        => Data,
                 Attachments => Attachments,
                 Headers     => Headers);

      Close (Connection);

      return Result;
   exception
      when others =>
         Close (Connection);
         raise;
   end SOAP_Post;

   procedure SOAP_Post
     (Connection  : HTTP_Connection;
      Result      : out Response.Data;
      SOAPAction  : String;
      Data        : String;
      Streaming   : Boolean         := False;
      Attachments : Attachment_List := Empty_Attachment_List;
      Headers     : Header_List     := Empty_Header_List)
   is
      Saved_Streaming : constant Boolean := Connection.Streaming;
   begin
      Connection.Self.Streaming := Streaming;

      --  For large payload (> 100kb) we use a slow version but safe
      --  version of To_Stream_Element_Array which uses the heap.

      if Data'Length > 100 * 1_024 then
         declare
            SEA_Data : Utils.Stream_Element_Array_Access :=
                         AWS.Translator.To_Stream_Element_Array (Data);
         begin
            Internal_Post
              (Connection   => Connection.Self.all,
               Result       => Result,
               Data         => SEA_Data.all,
               URI          => No_Data,
               SOAPAction   => SOAPAction,
               Content_Type => MIME.Text_XML,
               Attachments  => Attachments,
               Headers      => Headers);

            Utils.Unchecked_Free (SEA_Data);
         exception
            when others =>
               Utils.Unchecked_Free (SEA_Data);
         end;

      else
         Internal_Post
           (Connection   => Connection.Self.all,
            Result       => Result,
            Data         => AWS.Translator.To_Stream_Element_Array (Data),
            URI          => No_Data,
            SOAPAction   => SOAPAction,
            Content_Type => MIME.Text_XML,
            Attachments  => Attachments,
            Headers      => Headers);
      end if;

      Connection.Self.Streaming := Saved_Streaming;
   end SOAP_Post;

   --------------------
   -- SSL_Session_Id --
   --------------------

   function SSL_Session_Id (Connection : HTTP_Connection) return String is
   begin
      return Net.SSL.Session_Id_Image (Connection.SSL_Session);
   end SSL_Session_Id;

   --------------
   -- Timeouts --
   --------------

   function Timeouts
     (Connect  : Duration := Net.Forever;
      Send     : Duration := Net.Forever;
      Receive  : Duration := Net.Forever;
      Response : Duration := Net.Forever) return Timeouts_Values is
   begin
      return (Connect  => Connect,
              Send     => Send,
              Receive  => Receive,
              Response => Ada.Real_Time.To_Time_Span (Response));
   end Timeouts;

   function Timeouts (Each : Duration) return Timeouts_Values is
   begin
      return (Response => Ada.Real_Time.To_Time_Span (Each), others => Each);
   end Timeouts;

   ------------
   -- Upload --
   ------------

   procedure Upload
     (Connection : in out HTTP_Connection;
      Result     : out Response.Data;
      Filename   : String;
      URI        : String      := No_Data;
      Headers    : Header_List := Empty_Header_List;
      Progress   : access procedure
                     (Total, Sent : Stream_Element_Offset) := null) is
   begin
      Internal_Upload (Connection, Result, Filename, URI, Headers, Progress);
   end Upload;

   function Upload
     (URL          : String;
      Filename     : String;
      User         : String          := No_Data;
      Pwd          : String          := No_Data;
      Proxy        : String          := No_Data;
      Proxy_User   : String          := No_Data;
      Proxy_Pwd    : String          := No_Data;
      Timeouts     : Timeouts_Values := No_Timeout;
      Headers      : Header_List     := Empty_Header_List;
      Progress     : access procedure
                       (Total, Sent : Stream_Element_Offset) := null;
      User_Agent   : String          := Default.User_Agent;
      HTTP_Version : HTTP_Protocol   := HTTP_Default)
      return Response.Data
   is
      Connection : HTTP_Connection;
      Result     : Response.Data;
   begin
      Create (Connection,
              URL, User, Pwd, Proxy, Proxy_User, Proxy_Pwd,
              Persistent   => False,
              Timeouts     => Timeouts,
              User_Agent   => User_Agent,
              HTTP_Version => HTTP_Version);

      Upload
        (Connection, Result, Filename,
         Headers => Headers, Progress => Progress);

      Close (Connection);

      return Result;
   exception
      when others =>
         Close (Connection);
         raise;
   end Upload;

end AWS.Client;
