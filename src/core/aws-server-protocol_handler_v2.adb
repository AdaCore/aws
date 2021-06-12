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

with Ada.Containers;
with Ada.Streams;
with Ada.Text_IO;

with AWS.Headers;
with AWS.Log;
with AWS.Messages;
with AWS.Net.Buffered;
with AWS.Server.Context;
with AWS.Server.HTTP_Utils;
with AWS.Server.Status;
with AWS.Status.Set;
with AWS.Translator;
with AWS.Utils;

with AWS.HTTP2.Connection;
with AWS.HTTP2.Frame.GoAway;
with AWS.HTTP2.Frame.List;
with AWS.HTTP2.Frame.Settings;
with AWS.HTTP2.Frame.Settings;
with AWS.HTTP2.Frame.Window_Update;
with AWS.HTTP2.HPACK.Table;
with AWS.HTTP2.Message;
with AWS.HTTP2.Stream.Set;

separate (AWS.Server)

--  Some implementation notes:
--
--  3.3 - in TLS mode both client & server must send a connection preface
--

procedure Protocol_Handler_V2 (LA : in out Line_Attribute_Record) is

   use Ada.Streams;
   use Ada.Text_IO;

   use AWS.Server.HTTP_Utils;

   use type AWS.Status.Protocol_State;
   use type HTTP2.Stream_Id;

   use all type HTTP2.Frame.Kind_Type;

   Answers : HTTP2.Frame.List.Object;

   procedure Handle_HTTP2_Settings;
   --  This handles the HTTP2-Settings header when doing an upgrade from HTTP/1
   --  to HTTP/2. The header name is HTTP2-Settings and the value is a base64
   --  encoded of a settings frame payload.

   procedure Handle_Control_Frame (Frame : HTTP2.Frame.Object'Class)
     with Pre => Frame.Stream_Id = 0;
   --  Handle a control frame (Frame id = 0)

   procedure Handle_Message (Stream : HTTP2.Stream.Object);

   function Handle_Message
     (Status    : in out AWS.Status.Data;
      Stream_Id : HTTP2.Stream.Id) return AWS.HTTP2.Frame.List.Object;

   procedure Queue_Settings_Frame;
   --  Queue server settings frame (default configuration)

   procedure Set_Status (Status : in out AWS.Status.Data);
   --  Set standard/common status data for the response

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

   Sock_Ptr     : constant Socket_Access :=
                    LA.Server.Slots.Get (Index => LA.Line).Sock;

   Will_Close   : Boolean := True;
   --  Will_Close is set to true when the connection will be closed by the
   --  server. It means that the server is about to send the latest message
   --  to the client using this socket. The value will be changed by
   --  Set_Close_Status.

   Free_Slots   : Natural := 0;

   Settings : aliased HTTP2.Connection.Object;
   --  Connection settings

   H_Table  : aliased HTTP2.HPACK.Table.Object;
   --  ??? this table is create for a connection. we probably want to do better
   --  ??? and maybe set the pointer to this table into a frame object as it
   --  ??? is needed (and passed as parameter) for header & continuation
   --  ??? frame.

   Ctx : Context.Object (LA.Server, LA.Line, H_Table'Access, Settings'Access);

   --------------------------
   -- Handle_Control_Frame --
   --------------------------

   procedure Handle_Control_Frame (Frame : HTTP2.Frame.Object'Class) is

      procedure Handle (Frame : HTTP2.Frame.Settings.Object);
      --  Handle settings frame values

      procedure Handle (Frame : HTTP2.Frame.Window_Update.Object);
      --  Handle window update frame value

      ------------
      -- Handle --
      ------------

      procedure Handle (Frame : HTTP2.Frame.Settings.Object) is
      begin
         Settings.Set (Frame.Values);
      end Handle;

      procedure Handle (Frame : HTTP2.Frame.Window_Update.Object) is
      begin
         Settings.Set (Frame.Size_Increment);
      end Handle;

   begin
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
                             (AWS.Status.Header (LA.Stat), "HTTP2-Settings");
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
     (Status    : in out AWS.Status.Data;
      Stream_Id : HTTP2.Stream.Id) return AWS.HTTP2.Frame.List.Object
   is
      use type Response.Data_Mode;

      function Create_Message
        (H : Headers.List;
         R : in out Response.Data) return HTTP2.Message.Object;
      --  Create a response message object for the given headers and response
      --  data.

      --------------------
      -- Create_Message --
      --------------------

      function Create_Message
        (H : Headers.List;
         R : in out Response.Data) return HTTP2.Message.Object is
      begin
         --  Add needed content type

         if Response.Mode (R) = Response.File then
            return HTTP2.Message.Create
              (Headers  => H,
               Filename => Response.Filename (R));
         else
            return HTTP2.Message.Create
              (Headers => H,
               Payload => Response.Message_Body (R));
         end if;
      end Create_Message;

   begin
      if Extended_Log then
         AWS.Log.Set_Field
           (LA.Server.Log, LA.Log_Data,
            "c-ip", Net.Peer_Addr (Sock_Ptr.all));

         AWS.Log.Set_Field
           (LA.Server.Log, LA.Log_Data,
            "c-port", Utils.Image (Net.Peer_Port (Sock_Ptr.all)));

         AWS.Log.Set_Field
           (LA.Server.Log, LA.Log_Data,
            "s-ip", Net.Get_Addr (Sock_Ptr.all));

         AWS.Log.Set_Field
           (LA.Server.Log, LA.Log_Data,
            "s-port", Utils.Image (Net.Get_Port (Sock_Ptr.all)));
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
         H : Headers.List := Response.Header (R);
      begin
         --  The general headers

         H.Add
           (Messages.Date_Token,
            Messages.To_HTTP_Date (Utils.GMT_Clock));

         declare
            Server : constant String :=
                       CNF.Server_Header (LA.Server.Properties);
         begin
            if Server /= "" then
               H.Add (Messages.Server_Token, Server);
            end if;
         end;

         if CNF.Session (LA.Server.Properties)
           and then AWS.Status.Session_Created (Status)
         then
            --  This is an HTTP connection with session but there is no session
            --  ID set yet. So, send cookie to client browser.

            H.Add
              (Messages.Set_Cookie_Token,
               CNF.Session_Name (LA.Server.Properties) & '='
               & Session.Image (AWS.Status.Session (Status))
               & "; path=/; Version=1");

            --  And the internal private session

            H.Add
              (Messages.Set_Cookie_Token,
               CNF.Session_Private_Name (LA.Server.Properties) & '='
               & AWS.Status.Session_Private (Status)
               & "; path=/; Version=1");
         end if;

         if Response.Mode (R) = Response.File then
            H.Add
              (Messages.Content_Type_Token,
               Response.Content_Type (R));
         end if;

         --  Create response Message

         Ctx.Status := LA.Stat;
         Ctx.Response := R;

         declare
            RM : constant HTTP2.Message.Object := Create_Message (H, R);
         begin
            return RM.To_Frames (Ctx, Stream_Id);
         end;
      end;
   end Handle_Message;

   procedure Handle_Message (Stream : HTTP2.Stream.Object) is
      M : constant HTTP2.Message.Object := Stream.Message (Ctx);
   begin
      Set_Status (LA.Stat);

      AWS.Status.Set.Headers (LA.Stat, M.Headers);

      --  And set the request information using an HTTP/1 request line format

      AWS.Server.HTTP_Utils.Parse_Request_Line
        (M.Headers.Get (Messages.Method_Token) & ' '
         & M.Headers.Get (Messages.Path2_Token)
         & " HTTP_2",
         LA.Stat);

      Answers.Append (Handle_Message (LA.Stat, Stream.Identifier));
   end Handle_Message;

   --------------------------
   -- Queue_Settings_Frame --
   --------------------------

   procedure Queue_Settings_Frame is
      SP : constant HTTP2.Frame.Settings.Payload :=
             (Id    => HTTP2.Frame.Settings.ENABLE_PUSH,
              Value => 0);
      --  Disable push support
   begin
      HTTP2.Frame.Settings.Create
        (HTTP2.Frame.Settings.Set'(1 => SP)).Send (Sock_Ptr.all);
   end Queue_Settings_Frame;

   ----------------
   -- Set_Status --
   ----------------

   procedure Set_Status (Status : in out AWS.Status.Data) is
   begin
      AWS.Status.Set.Reset (Status);

      --  Set status socket and peername

      AWS.Status.Set.Socket (Status, Sock_Ptr);

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
   --  Initialize the HPACK table to encode/decode headers

   H_Table.Init;

   LA.Log_Data := AWS.Log.Empty_Fields_Table;

   --  The first bytes on the connection should be a connection preface

   declare
      Connection_Preface : constant Stream_Element_Array :=
                             (16#50#, 16#52#, 16#49#, 16#20#, 16#2a#, 16#20#,
                              16#48#, 16#54#, 16#54#, 16#50#, 16#2f#, 16#32#,
                              16#2e#, 16#30#, 16#0d#, 16#0a#, 16#0d#, 16#0a#,
                              16#53#, 16#4d#, 16#0d#, 16#0a#, 16#0d#, 16#0a#);
      Preface            : Stream_Element_Array (1 .. 24);
   begin
      Net.Buffered.Read (Sock_Ptr.all, Preface);

      if Preface = Connection_Preface then
         Put_Line ("OK connection preface");
         Put_Line ("Switched in v2 protocol...");
      else
         raise Constraint_Error with "connection preface not found";
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
      declare
         Frame : constant HTTP2.Frame.Object'Class :=
                   HTTP2.Frame.Read (Sock_Ptr.all);
      begin
         Settings.Set (HTTP2.Frame.Settings.Object (Frame).Values);
      end;
   end if;

   --  Now send AWS settings frame

   Queue_Settings_Frame;

   --  The maximum number of simultaneous stream has now been negociated

   declare
      Max_Stream : constant Containers.Count_Type :=
                     Containers.Count_Type (Settings.Max_Concurrent_Streams);
      S          : HTTP2.Stream.Set.Object (Max_Stream);
   begin
      --  We now need to answer to the request made during the upgrade

      if AWS.Status.Protocol (LA.Stat) = AWS.Status.H2C then
         H2C_Answer := Handle_Message (LA.Stat, 1);
      end if;

      For_Every_Frame : loop
         --  Send back frames if any waiting

         while not Answers.Is_Empty loop
            declare
               Frame     : constant HTTP2.Frame.Object'Class :=
                             Answers.First_Element;
               Stream_Id : constant HTTP2.Stream.Id := Frame.Stream_Id;
            begin
               LA.Server.Slots.Mark_Phase (LA.Line, Server_Response);

               Answers.Delete_First;

               Frame.Dump ("SEND");

               if Stream_Id = 0 then
                  Frame.Send (Sock_Ptr.all);
               else
                  S (Stream_Id).Send_Frame (Frame);
               end if;
            end;
         end loop;

         --  Get frame

         LA.Server.Slots.Mark_Phase (LA.Line, Wait_For_Client);

         declare
            use type HTTP2.Error_Codes;

            Frame     : constant HTTP2.Frame.Object'Class :=
                          HTTP2.Frame.Read (Sock_Ptr.all);
            Stream_Id : constant HTTP2.Stream_Id := Frame.Stream_Id;
         begin
            delay 0.5;

            Frame.Dump ("GET");

            --  if a GOAWAY frame is received we need to exit now. Then
            --  check if the frame is valid, if not we need to send back a
            --  GOAWAY immediatly.

            if Frame.Kind = K_GoAway then
               Will_Close := True;

               exit For_Every_Frame;

            elsif Frame.Validate /= HTTP2.C_No_Error then
               --  Send a GOAWAY response right now

               Put_Line ("FRAME ERROR: " & Frame.Validate'Img);

               HTTP2.Frame.GoAway.Create
                 (Stream_Id => Frame.Stream_Id,
                  Error     => Frame.Validate).Send (Sock_Ptr.all);

               Will_Close := True;

               exit For_Every_Frame;

            elsif Stream_Id = 0 then
               Handle_Control_Frame (Frame);

            else
               if not S.Contains (Stream_Id) then
                  S.Insert
                    (Stream_Id,
                     HTTP2.Stream.Create (Sock_Ptr,  Stream_Id));
               end if;

               begin
                  S (Stream_Id).Received_Frame (Frame);

                  Put_Line
                    ("S " & Stream_Id'Img & " - " & S (Stream_Id).State'Img);

                  if S (Stream_Id).Is_Message_Ready then
                     Handle_Message (S (Stream_Id));
                  end if;
               exception
                  when HTTP2.Protocol_Error =>
                     HTTP2.Frame.GoAway.Create
                       (Stream_Id => Frame.Stream_Id,
                        Error     =>
                           HTTP2.C_Protocol_Error).Send (Sock_Ptr.all);
               end;
            end if;
         end;

         if not H2C_Answer.Is_Empty then
            Answers.Append (H2C_Answer);
            H2C_Answer.Clear;
         end if;
      end loop For_Every_Frame;
   end;

   --  Release memory for local objects

   AWS.Status.Set.Free (LA.Stat);

exception
   when Net.Socket_Error =>
      Will_Close := True;
      AWS.Status.Set.Free (LA.Stat);

   when E : others =>
      AWS.Log.Write
        (LA.Server.Error_Log,
         LA.Stat,
         "Exception handler bug "
         & Utils.CRLF_2_Spaces
           (Ada.Exceptions.Exception_Information (E)));

      Will_Close := True;
      AWS.Status.Set.Free (LA.Stat);
end Protocol_Handler_V2;
