------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

--  This procedure is responsible of handling the HTTP protocol. Every
--  responses and incoming requests are parsed/formated here.

with Ada.Characters.Handling;
with Ada.Containers;
with Ada.Streams;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;

with AWS.Headers;
with AWS.Log;
with AWS.Messages;
with AWS.Net.Buffered;
with AWS.Response.Set;
with AWS.Server.Context;
with AWS.Server.HTTP_Utils;
with AWS.Server.Status;
with AWS.Status.Set;
with AWS.Translator;
with AWS.Utils;

with AWS.HTTP2.Connection;
with AWS.HTTP2.Frame.GoAway;
with AWS.HTTP2.Frame.List;
with AWS.HTTP2.Frame.Ping;
with AWS.HTTP2.Frame.RST_Stream;
with AWS.HTTP2.Frame.Settings;
with AWS.HTTP2.Frame.Settings;
with AWS.HTTP2.Frame.Window_Update;
with AWS.HTTP2.HPACK.Table;
with AWS.HTTP2.Message.List;
with AWS.HTTP2.Stream.Set;

separate (AWS.Server)

--  Some implementation notes:
--
--  3.3 - in TLS mode both client & server must send a connection preface
--

procedure Protocol_Handler_V2 (LA : in out Line_Attribute_Record) is

   use Ada.Streams;

   use AWS.Server.HTTP_Utils;

   use type AWS.Status.Protocol_State;
   use type HTTP2.Stream_Id;

   use all type HTTP2.Frame.Kind_Type;

   procedure Handle_HTTP2_Settings;
   --  This handles the HTTP2-Settings header when doing an upgrade from HTTP/1
   --  to HTTP/2. The header name is HTTP2-Settings and the value is a base64
   --  encoded of a settings frame payload.

   procedure Handle_Control_Frame
     (Frame   : HTTP2.Frame.Object'Class;
      Streams : in out HTTP2.Stream.Set.Object;
      Error   : out HTTP2.Error_Codes)
     with Pre => Frame.Stream_Id = 0;
   --  Handle a control frame (Frame id = 0)

   procedure Handle_Message (Stream : HTTP2.Stream.Object);

   --  function Handle_Message
   --    (Status : in out AWS.Status.Data;
   --     Stream : HTTP2.Stream.Object) return AWS.HTTP2.Frame.List.Object;
   function Handle_Message
     (Status : in out AWS.Status.Data;
      Stream : HTTP2.Stream.Object) return AWS.HTTP2.Message.Object;

   procedure Queue_Settings_Frame;
   --  Queue server settings frame (default configuration)

   procedure Set_Status (Status : in out AWS.Status.Data);
   --  Set standard/common status data for the response

   procedure Finalize;
   --  Free resources on return from Protocol_Handler_V2

   Finalizer : AWS.Utils.Finalizer (Finalize'Access) with Unreferenced;

   Case_Sensitive_Parameters : constant Boolean :=
                                 CNF.Case_Sensitive_Parameters
                                   (LA.Server.Properties);

   Extended_Log     : constant Boolean :=
                        CNF.Log_Extended_Fields_Length
                          (LA.Server.Properties) > 0;

   Multislots       : constant Boolean :=
                        CNF.Max_Connection (LA.Server.Properties) > 1;

   Keep_Alive_Limit : constant Natural :=
                        CNF.Free_Slots_Keep_Alive_Limit (LA.Server.Properties);

   Sock             : constant Socket_Access :=
                        LA.Server.Slots.Get (Index => LA.Line).Sock;

   Will_Close   : Boolean := True;
   --  Will_Close is set to true when the connection will be closed by the
   --  server. It means that the server is about to send the latest message
   --  to the client using this socket. The value will be changed by
   --  Set_Close_Status.

   Free_Slots : Natural := 0;

   Settings   : aliased HTTP2.Connection.Object;
   --  Connection settings

   Deferred_Messages : HTTP2.Message.List.Object;
   --  Deferreed messages waiting to be sent when the flow control window will
   --  allow.

   Answers    : HTTP2.Frame.List.Object;
   --  Set of frames to be sent

   Tab_Dec : aliased HTTP2.HPACK.Table.Object;
   Tab_Enc : aliased HTTP2.HPACK.Table.Object;
   --  ??? this table is create for a connection. we probably want to do better
   --  ??? and maybe set the pointer to this table into a frame object as it
   --  ??? is needed (and passed as parameter) for header & continuation
   --  ??? frame.

   Ctx : Context.Object
     (LA.Server, LA.Line, Tab_Enc'Access, Tab_Dec'Access, Settings'Access);

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
   begin
      AWS.Status.Set.Free (LA.Stat);
   end Finalize;

   --------------------------
   -- Handle_Control_Frame --
   --------------------------

   procedure Handle_Control_Frame
     (Frame   : HTTP2.Frame.Object'Class;
      Streams : in out HTTP2.Stream.Set.Object;
      Error   : out HTTP2.Error_Codes)
   is

      procedure Handle (Frame : HTTP2.Frame.Settings.Object);
      --  Handle settings frame values

      procedure Handle (Frame : HTTP2.Frame.Window_Update.Object);
      --  Handle window update frame value

      package S renames HTTP2.Frame.Settings;

      ------------
      -- Handle --
      ------------

      procedure Handle (Frame : HTTP2.Frame.Settings.Object) is
         use type S.Settings_Kind;

         Initial_Window_Size : constant Integer :=
                                 Settings.Initial_Window_Size;
      begin
         Settings.Set (Frame.Values);

         --  Update the control flow window size of all streams

         for V of Frame.Values loop
            if V.Id = S.INITIAL_WINDOW_SIZE then
               declare
                  Value : constant Natural := Integer (V.Value);
                  Incr  : constant Integer := Value
                                                - Initial_Window_Size;
               begin
                  for S of Streams loop
                     if HTTP2.Connection.Flow_Control_Window_Valid
                       (S.Flow_Control_Window, Value)
                     then
                        S.Update_Flow_Control_Window (Incr);
                     end if;
                  end loop;
               end;
            end if;
         end loop;
      end Handle;

      procedure Handle (Frame : HTTP2.Frame.Window_Update.Object) is
         Incr : constant Natural := Natural (Frame.Size_Increment);
      begin
         if HTTP2.Connection.Flow_Control_Window_Valid
           (Settings.Flow_Control_Window, Incr)
         then
            Settings.Update_Flow_Control_Window
              (Natural (Frame.Size_Increment));
         else
            Error := HTTP2.C_Flow_Control_Error;
            return;
         end if;
      end Handle;

   begin
      Error := HTTP2.C_No_Error;

      if Frame.Kind = K_Ping
        and then not Frame.Has_Flag (HTTP2.Frame.Ack_Flag)
      then
         --  A probing ping frame, respond now with the same
         --  payload (see RFC-7540 / 6.7).

         declare
            R_Ping : HTTP2.Frame.Object'Class := Frame;
         begin
            R_Ping.Set_Flags (HTTP2.Frame.Ack_Flag);
            Answers.Prepend (R_Ping);
         end;

      elsif Frame.Kind = K_Settings
        and then not Frame.Has_Flag (HTTP2.Frame.Ack_Flag)
      then
         Handle (HTTP2.Frame.Settings.Object (Frame));
         Answers.Prepend (HTTP2.Frame.Settings.Ack);

      elsif Frame.Kind = K_Settings
        and then Frame.Has_Flag (HTTP2.Frame.Ack_Flag)
      then
         Answers.Prepend (HTTP2.Frame.Settings.Ack);

      elsif Frame.Kind = K_Window_Update then
         Handle (HTTP2.Frame.Window_Update.Object (Frame));
      end if;
   end Handle_Control_Frame;

   ---------------------------
   -- Handle_HTTP2_Settings --
   ---------------------------

   procedure Handle_HTTP2_Settings is
      HTTP2_Settings   : constant String :=
                           AWS.Headers.Get_Values
                             (AWS.Status.Header (LA.Stat),
                              Messages.HTTP2_Settings);
      Settings_Payload : constant Stream_Element_Array :=
                           Translator.Base64_Decode (HTTP2_Settings);
      Frame            : constant HTTP2.Frame.Settings.Object :=
                           HTTP2.Frame.Settings.Create (Settings_Payload);
   begin
      Settings.Set (Frame.Values);
   end Handle_HTTP2_Settings;

   --------------------
   -- Handle_Message --
   --------------------

   function Handle_Message
     (Status : in out AWS.Status.Data;
      Stream : HTTP2.Stream.Object) return AWS.HTTP2.Message.Object
   is
      use type Response.Data_Mode;

   begin
      if Extended_Log then
         AWS.Log.Set_Field
           (LA.Server.Log, LA.Log_Data,
            "c-ip", Net.Peer_Addr (Sock.all));

         AWS.Log.Set_Field
           (LA.Server.Log, LA.Log_Data,
            "c-port", Utils.Image (Net.Peer_Port (Sock.all)));

         AWS.Log.Set_Field
           (LA.Server.Log, LA.Log_Data,
            "s-ip", Net.Get_Addr (Sock.all));

         AWS.Log.Set_Field
           (LA.Server.Log, LA.Log_Data,
            "s-port", Utils.Image (Net.Get_Port (Sock.all)));
      end if;

      --  If there is no more slot available and we have many
      --  of them, try to abort one of them.

      LA.Server.Slots.Increment_Slot_Activity_Counter (LA.Line, Free_Slots);

      if Multislots and then Free_Slots = 0 then
         Force_Clean (LA.Server.all);
      end if;

      if Extended_Log then
         AWS.Log.Set_Field
           (LA.Server.Log, LA.Log_Data,
            "s-free-slots", Utils.Image (Free_Slots));
      end if;

      LA.Server.Slots.Mark_Phase (LA.Line, Client_Data);

      declare
         R : Response.Data :=
               Server.HTTP_Utils.Build_Answer (LA.Server.all, Status);

         procedure Add_Header (Name, Value : String) with Inline;
         --  Add header into response R

         -----------------
         --  Add_Header --
         -----------------

         procedure Add_Header (Name, Value : String) is
         begin
            Response.Set.Add_Header (R, Name, Value);
         end Add_Header;

      begin
         --  The general headers

         Add_Header
           (Messages.Date_Token,
            Messages.To_HTTP_Date (Utils.GMT_Clock));

         declare
            Server : constant String :=
                       CNF.Server_Header (LA.Server.Properties);
         begin
            if Server /= "" then
               Add_Header (Messages.Server_Token, Server);
            end if;
         end;

         if CNF.Session (LA.Server.Properties)
           and then AWS.Status.Session_Created (Status)
         then
            --  This is an HTTP connection with session but there is no session
            --  ID set yet. So, send cookie to client browser.

            Add_Header
              (Messages.Set_Cookie_Token,
               CNF.Session_Name (LA.Server.Properties) & '='
               & Session.Image (AWS.Status.Session (Status))
               & "; path=/; Version=1");

            --  And the internal private session

            Add_Header
              (Messages.Set_Cookie_Token,
               CNF.Session_Private_Name (LA.Server.Properties) & '='
               & AWS.Status.Session_Private (Status)
               & "; path=/; Version=1");
         end if;

         if Response.Mode (R) = Response.File then
            Add_Header
              (Messages.Content_Type_Token,
               Response.Content_Type (R));
         end if;

         --  Create response Message

         Ctx.Status := LA.Stat;
         Ctx.Response := R;

         return HTTP2.Message.Create (R, Stream.Identifier);
      end;
   end Handle_Message;

   procedure Handle_Message (Stream : HTTP2.Stream.Object) is

      use type HTTP2.Error_Codes;

      procedure Validate_Headers
        (Headers : AWS.Headers.List;
         Error   : out HTTP2.Error_Codes);
      --  Validate headers name as required by HTTP/2

      ----------------------
      -- Validate_Headers --
      ----------------------

      procedure Validate_Headers
        (Headers : AWS.Headers.List;
         Error   : out HTTP2.Error_Codes)
      is
         use Ada.Strings;
         use Ada.Strings.Fixed;

         Header_Found : Boolean := False;

         type Pseudo_Enum is (Method, Scheme, Path);
         --  Pseudo headers have to be exactly one

         Has_Pseudo : array (Pseudo_Enum) of Boolean := (others => False);

      begin
         Error := HTTP2.C_No_Error;

         for K in 1 .. Headers.Count loop
            declare
               Header : constant String := Headers.Get_Name (K);
               Value  : constant String := Headers.Get_Value (K);
            begin
               if Header'Length > 1 and then Header (Header'First) = ':' then
                  --  Pseudo header

                  if Header_Found then
                     AWS.Log.Write
                       (LA.Server.Error_Log, LA.Stat,
                        "pseudo headers must appear first");
                     Error := HTTP2.C_Protocol_Error;
                     return;
                  end if;

                  if Header in Messages.Method_Token | Messages.Scheme_Token
                             | Messages.Path2_Token
                  then
                     declare
                        PH : constant Pseudo_Enum :=
                               Pseudo_Enum'Value
                                 (Header (Header'First + 1 .. Header'Last));
                     begin
                        if Has_Pseudo (PH) then
                           AWS.Log.Write
                             (LA.Server.Error_Log, LA.Stat,
                              "duplicate " & Header & " pseudo header");
                           Error := HTTP2.C_Protocol_Error;
                           return;
                        end if;

                        Has_Pseudo (PH) := True;

                        if PH = Path and then Value = "" then
                           AWS.Log.Write
                             (LA.Server.Error_Log, LA.Stat,
                              "empty header path should be rejected");
                           Error := HTTP2.C_Protocol_Error;
                           return;
                        end if;
                     end;

                  elsif Header /= ":authority" then
                     AWS.Log.Write
                       (LA.Server.Error_Log, LA.Stat,
                        "unknown pseudo header " & Header);
                     Error := HTTP2.C_Protocol_Error;
                     return;
                  end if;

               else
                  Header_Found := True;
               end if;

               if Index (Header, Maps.Constants.Upper_Set) /= 0 then
                  AWS.Log.Write
                    (LA.Server.Error_Log, LA.Stat, "no upper case allowed");
                  Error := HTTP2.C_Protocol_Error;
                  return;

               elsif Header = "connection" then
                  AWS.Log.Write
                    (LA.Server.Error_Log, LA.Stat,
                     "no connection specific header allowed");
                  Error := HTTP2.C_Protocol_Error;
                  return;

               elsif Header = "te" and then Value /= "trailers" then
                  AWS.Log.Write
                    (LA.Server.Error_Log, LA.Stat,
                     "no TE header except with single value ""trailers""");
                  Error := HTTP2.C_Protocol_Error;
                  return;
               end if;
            end;
         end loop;

         --  Check if mandatory pseudo headers are present

         for PH in Has_Pseudo'Range loop
            if not Has_Pseudo (PH) then
               AWS.Log.Write
                 (LA.Server.Error_Log, LA.Stat,
                  "pseudo header :"
                  & Ada.Characters.Handling.To_Lower (PH'Img)
                  & " must exists");
               Error := HTTP2.C_Protocol_Error;
            end if;
         end loop;
      end Validate_Headers;

      Headers : constant AWS.Headers.List := Stream.Headers;
      Error   : HTTP2.Error_Codes;
   begin
      Set_Status (LA.Stat);

      AWS.Status.Set.Headers (LA.Stat, Headers);

      Stream.Append_Body (LA.Stat);

      --  Check headers' validity

      Validate_Headers (Headers, Error);

      if Error /= HTTP2.C_No_Error then
         raise HTTP2.Protocol_Error with
           HTTP2.Exception_Message (Error, "headers validity check fails");
      end if;

      --  And set the request information using an HTTP/1 request line format

      declare
         Path        : constant String := Headers.Get (Messages.Path2_Token);
         Path_Last   : Positive;
         Query_First : Positive;

      begin
         HTTP_Utils.Split_Path (Path, Path_Last, Query_First);

         AWS.Status.Set.Request
           (LA.Stat,
            Method       => Headers.Get (Messages.Method_Token),
            URI          => Path (Path'First .. Path_Last),
            HTTP_Version => HTTP_2);

         AWS.Status.Set.Query
           (LA.Stat,
            Parameters => Path (Query_First .. Path'Last));
      end;

      Deferred_Messages.Append (Handle_Message (LA.Stat, Stream));
   end Handle_Message;

   --------------------------
   -- Queue_Settings_Frame --
   --------------------------

   procedure Queue_Settings_Frame is
      use HTTP2.Frame.Settings;
   begin
      Create (To_Set (LA.Server.Properties)).Send (Sock.all);
   end Queue_Settings_Frame;

   ----------------
   -- Set_Status --
   ----------------

   procedure Set_Status (Status : in out AWS.Status.Data) is
   begin
      AWS.Status.Set.Reset (Status);

      --  Set status socket and peername

      AWS.Status.Set.Socket (Status, Sock);

      AWS.Status.Set.Case_Sensitive_Parameters
        (Status, Case_Sensitive_Parameters);

      --  Need to set the request-line (POST, GET...)

      AWS.Status.Set.Connection_Data
        (Status,
         CNF.Server_Host (LA.Server.Properties),
         AWS.Server.Status.Port (LA.Server.all),
         CNF.Security (LA.Server.Properties));

      Set_Close_Status
        (Status,
         Keep_Alive => Free_Slots >= Keep_Alive_Limit,
         Will_Close => Will_Close);

      AWS.Status.Set.Keep_Alive (Status, not Will_Close);
   end Set_Status;

   H2C_Answer : AWS.HTTP2.Frame.List.Object;

begin
   --  Initialize the HPACK tables to encode/decode headers

   LA.Log_Data := AWS.Log.Empty_Fields_Table;

   HTTP2.Connection.Set (Settings, LA.Server.Properties);

   --  The first bytes on the connection should be a connection preface

   declare
      Connection_Preface : constant Stream_Element_Array :=
                             (16#50#, 16#52#, 16#49#, 16#20#, 16#2a#, 16#20#,
                              16#48#, 16#54#, 16#54#, 16#50#, 16#2f#, 16#32#,
                              16#2e#, 16#30#, 16#0d#, 16#0a#, 16#0d#, 16#0a#,
                              16#53#, 16#4d#, 16#0d#, 16#0a#, 16#0d#, 16#0a#);
      Preface            : Stream_Element_Array (1 .. 24);
   begin
      Net.Buffered.Read (Sock.all, Preface);

      if Preface /= Connection_Preface then
         HTTP2.Frame.GoAway.Create
           (Stream_Id => 0,
            Error     => HTTP2.C_Protocol_Error).Send (Sock.all);
         raise HTTP2.Protocol_Error with
           HTTP2.Exception_Message
             (HTTP2.C_Protocol_Error, "wrong connection preface");
      end if;
   end;

   --  Handle the settings frame now. There is two cases:
   --  1. when upgrading from HTTP/1 (h2c) handle HTTP2-Settings payload
   --     (a setting frame).
   --  2. Read the first frame which should be the settings frame if using h2.

   if AWS.Status.Protocol (LA.Stat) = AWS.Status.H2C then
      Handle_HTTP2_Settings;

   else
      --  First frame should be a setting frame

      Settings.Set (Sock.all);
   end if;

   --  Now send AWS settings frame

   Queue_Settings_Frame;

   --  The maximum number of simultaneous stream has now been negociated

   declare
      use HTTP2;
      use type HTTP2.Stream.State_Kind;

      Max_Stream : constant Containers.Count_Type :=
                     Containers.Count_Type (Settings.Max_Concurrent_Streams);
      S          : HTTP2.Stream.Set.Object (Max_Stream);
      Last_SID   : HTTP2.Stream.Id := 0;
      In_Header  : Boolean := False;
      --  Need to avoid unknown extension frame in the middle of a header block
      --  (RFC 7450, 5.5).

      procedure Go_Away (Error : HTTP2.Error_Codes; Message : String);
      --  Send GoAway frame, log message to error log and set Will_Close flag
      --  to True.

      -------------
      -- Go_Away --
      -------------

      procedure Go_Away (Error : HTTP2.Error_Codes; Message : String) is
      begin
         if Message /= "" then
            AWS.Log.Write (LA.Server.Error_Log, LA.Stat, Message);
         end if;

         HTTP2.Frame.GoAway.Create
           (Stream_Id => Last_SID, Error => Error).Send (Sock.all);

         Will_Close := True;
      end Go_Away;


   begin
      --  We now need to answer to the request made during the upgrade

      if AWS.Status.Protocol (LA.Stat) = AWS.Status.H2C then
         S.Insert
           (1,
            HTTP2.Stream.Create (Sock,  1, Settings.Flow_Control_Window));

         declare
            M : HTTP2.Message.Object := Handle_Message (LA.Stat, S (1));
         begin
            H2C_Answer := M.To_Frames (Ctx, S (1));
         end;
      end if;

      For_Every_Frame : loop
         if not Deferred_Messages.Is_Empty then
            declare
               M : HTTP2.Message.Object :=
                     Deferred_Messages.First_Element;
            begin
               if S (M.Stream_Id).Flow_Control_Window > 0 then
                  Deferred_Messages.Delete_First;

                  --  Sends as much frame as possible that conform with the
                  --  current flow control window limit.

                  Answers.Append (M.To_Frames (Ctx, S (M.Stream_Id)));

                  --  If some more data are available, register back the
                  --  message to send corresponding remaining data frames.

                  if M.More_Frames then
                     Deferred_Messages.Prepend (M);
                  end if;
               end if;
            end;
         end if;

         --  Send back frames if any waiting

         while not Answers.Is_Empty loop
            declare
               Frame     : constant HTTP2.Frame.Object'Class :=
                             Answers.First_Element;
               Stream_Id : constant HTTP2.Stream.Id := Frame.Stream_Id;
            begin
               LA.Server.Slots.Mark_Phase (LA.Line, Server_Response);

               if Stream_Id = 0 then
                  Frame.Send (Sock.all);

               else
                  exit when Frame.Kind = K_Data
                    and then
                      (Settings.Flow_Control_Window < Integer (Frame.Length)
                       or else S (Stream_Id).Flow_Control_Window <
                             Integer (Frame.Length));

                  S (Stream_Id).Send_Frame (Frame);

                  --  Update connection Flow Control Window

                  if Frame.Kind = K_Data then
                     Settings.Update_Flow_Control_Window
                       (-Natural (Frame.Length));

                     if Frame.Has_Flag (HTTP2.Frame.End_Stream_Flag) then
                        Log_Commit
                          (Ctx.HTTP.all, Ctx.Response, Ctx.Status,
                           S (Stream_Id).Bytes_Sent);
                     end if;
                  end if;
               end if;

               Answers.Delete_First;
            end;
         end loop;

         --  Get frame

         LA.Server.Slots.Mark_Phase (LA.Line, Wait_For_Client);

         declare
            use type HTTP2.Error_Codes;
            use type Ada.Containers.Count_Type;

            Frame      : constant HTTP2.Frame.Object'Class :=
                           HTTP2.Frame.Read (Sock.all, Settings);
            Stream_Id  : constant HTTP2.Stream_Id := Frame.Stream_Id;
            Prev_State : Stream.State_Kind;
            Error      : Error_Codes;
         begin
            if HTTP2.Debug then
               Frame.Dump ("GET");
            end if;

            --  First check that the possibly new stream id is not below
            --  last stream id.

            if Frame.Kind = K_Invalid then
               --  Check if the frame is valid, if not we
               --  need to send back a GOAWAY immediatly.

               if In_Header then
                  --  Unknown extension frame in the middle of a header block
                  --  (RFC 7450, 5.5).

                  Go_Away
                    (C_Protocol_Error,
                     "unknown frame in the middle of a header block");

                  exit For_Every_Frame;

               else
                  HTTP2.Frame.Ping.Create
                    (Flags => HTTP2.Frame.End_Stream_Flag).Send (Sock.all);
               end if;

            elsif Frame.Kind = K_Settings
              and then HTTP2.Frame.Settings.Object (Frame).Is_Ignored
            then
               --  The case when all settings parameters are unknown.
               --  An endpoint that receives a SETTINGS frame with any unknown
               --  or unsupported identifier MUST ignore that setting.
               --  (RFC 7540, 6.5.2.).

               HTTP2.Frame.Ping.Create
                 (Flags => HTTP2.Frame.End_Stream_Flag).Send (Sock.all);

            elsif Frame.Kind = K_Push_Promise then
               --  A server cannot receive a push-promise frame

               Go_Away
                 (C_Protocol_Error,
                  "server cannot receive a push-promise frame");

               exit For_Every_Frame;

            elsif not Frame.Is_Defined then
               Go_Away (C_Protocol_Error, "empty frame " & Frame.Kind'Img);

               exit For_Every_Frame;

            elsif not Frame.Is_Valid (Settings, Error) then
               --  Send a GOAWAY response right now

               Go_Away (Error, "invalid frame " & Frame.Kind'Img);

               exit For_Every_Frame;

            elsif Frame.Kind = K_GoAway then
               --  if a GOAWAY frame is received we need to exit now

               Will_Close := True;
               exit For_Every_Frame;

            elsif Stream_Id = 0 then
               Handle_Control_Frame (Frame, S, Error);

               if Error /= HTTP2.C_No_Error then
                  Go_Away (Error, "invalid control frame " & Frame.Kind'Img);

                  exit For_Every_Frame;
               end if;

            else
               In_Header := Frame.Kind in K_Headers | K_Continuation
                 and then not Frame.Has_Flag (HTTP2.Frame.End_Headers_Flag);
               --  Need to avoid unknown extension frame in the middle of a
               --  header block (RFC 7450, 5.5).

               if not S.Contains (Stream_Id) then
                  --  A new stream, check that Id is greater than last
                  --  stream.

                  if (Frame.Kind /= K_Priority and then Stream_Id < Last_SID)
                    or else S.Capacity = S.Length
                  then
                     Go_Away
                       (C_Protocol_Error, "too many streams were opened");
                     exit For_Every_Frame;

                  else
                     if Frame.Kind /= K_Priority then
                        Last_SID := Stream_Id;
                     end if;

                     S.Insert
                       (Stream_Id,
                        HTTP2.Stream.Create
                          (Sock, Stream_Id, Settings.Initial_Window_Size));
                  end if;
               end if;

               --  Keep current stream state before changing the state to
               --  conform to live-cycle. (RFC 7540 5.1).

               Prev_State := S (Stream_Id).State;

               S (Stream_Id).Received_Frame (Ctx, Frame, Error);

               if Error /= C_No_Error then
                  if Error = C_Flow_Control_Error then
                     HTTP2.Frame.RST_Stream.Create
                       (Stream_Id => Stream_Id,
                        Error     => Error).Send (Sock.all);
                     exit For_Every_Frame;

                  else
                     Go_Away (Error, "");

                     exit For_Every_Frame;
                  end if;
               end if;

               if S (Stream_Id).Is_Message_Ready
                 and then Prev_State < Stream.Half_Closed_Remote
               then
                  Handle_Message (S (Stream_Id));
               end if;
            end if;

         exception
            when E : Protocol_Error =>
               declare
                  Message : constant String := Exception_Message (E);
               begin
                  Go_Away (Exception_Code (Message), Message);
               end;

               exit For_Every_Frame;
         end;

         if not H2C_Answer.Is_Empty then
            Answers.Append (H2C_Answer);
            H2C_Answer.Clear;
         end if;
      end loop For_Every_Frame;
   end;

exception
   when Net.Socket_Error =>
      null;

   when HTTP2.Protocol_Error =>
      Will_Close := True;
      LA.Server.Slots.Mark_Phase (LA.Line, Server_Response);

   when E : others =>
      AWS.Log.Write
        (LA.Server.Error_Log,
         LA.Stat,
         "Exception handler bug "
         & Utils.CRLF_2_Spaces
           (Ada.Exceptions.Exception_Information (E)));

      Will_Close := True;
end Protocol_Handler_V2;
