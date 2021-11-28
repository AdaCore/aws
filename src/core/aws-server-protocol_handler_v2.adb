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
with Ada.Strings.Unbounded;

with AWS.Attachments;
with AWS.Headers.Values;
with AWS.Log;
with AWS.Messages;
with AWS.MIME;
with AWS.Net.Buffered;
with AWS.Parameters;
with AWS.Resources.Streams.Memory;
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
with AWS.HTTP2.Message.List;
with AWS.HTTP2.Stream.Set;

separate (AWS.Server)

--  Some implementation notes:
--
--  3.3 - in TLS mode both client & server must send a connection preface
--

procedure Protocol_Handler_V2
  (LA            : in out Line_Attribute_Record;
   Will_Close    : out Boolean;
   HTTP2_Ref     : HTTP2_Context_References.Ref;
   Check_Preface : Boolean := True)
is
   use Ada.Streams;

   use AWS.Server.HTTP_Utils;

   use type AWS.Status.Protocol_State;
   use type HTTP2.Stream_Id;
   use type HTTP2.Stream.Upload_State_Kind;

   use all type HTTP2.Frame.Kind_Type;

   procedure Handle_HTTP2_Settings;
   --  This handles the HTTP2-Settings header when doing an upgrade from HTTP/1
   --  to HTTP/2. The header name is HTTP2-Settings and the value is a base64
   --  encoded of a settings frame payload.

   procedure Handle_Control_Frame
     (Frame : HTTP2.Frame.Object'Class; Error : out HTTP2.Error_Codes)
     with Pre => Frame.Stream_Id = 0;
   --  Handle a control frame (Frame id = 0)

   procedure Handle_Message (Stream : in out HTTP2.Stream.Object);

   function Handle_Message
     (Stream : HTTP2.Stream.Object) return AWS.HTTP2.Message.Object;

   procedure Handle_Body (Stream : HTTP2.Stream.Object);

   procedure Queue_Settings_Frame;
   --  Queue server settings frame (default configuration)

   function Create_H2_Reference return H2CR.Ref;

   function Create_HTTP2_Context return Context.Object;

   Case_Sensitive_Parameters : constant Boolean :=
                                 CNF.Case_Sensitive_Parameters
                                   (LA.Server.Properties);

   Extended_Log     : constant Boolean :=
                        CNF.Log_Extended_Fields_Length
                          (LA.Server.Properties) > 0;

   Keep_Alive_Close_Limit : constant Natural :=
                              CNF.Keep_Alive_Close_Limit
                                (LA.Server.Properties);

   Sock             : constant Socket_Access :=
                        LA.Server.Slots.Get_Socket (Index => LA.Line);

   Free_Slots : Natural;

   Deferred_Messages : HTTP2.Message.List.Object;
   --  Deferreed messages waiting to be sent when the flow control window will
   --  allow.

   Answers    : HTTP2.Frame.List.Object;
   --  Set of frames to be sent

   Error_Answer : Response.Data;
   Request      : AWS.Status.Data renames LA.Stat.all;

   procedure Finalize;
   --  Free resources on return from Protocol_Handler_V2

   procedure Process_Error (E : Exception_Occurrence);
   --  Call LA.Server.Exception_Handler and Log_Commit in last exception
   --  handlers.

   Finalizer : AWS.Utils.Finalizer (Finalize'Access) with Unreferenced;
   S         : HTTP2.Stream.Set.Object;

   -------------------------
   -- Create_H2_Reference --
   -------------------------

   function Create_H2_Reference return H2CR.Ref is
   begin
      if HTTP2_Ref.Is_Null then
         return Result : H2CR.Ref do
            Result.Set (HTTP2_Context'(others => <>));
         end return;

      else
         return HTTP2_Ref;
      end if;
   end Create_H2_Reference;

   H2_Ref : constant H2CR.Ref := Create_H2_Reference;

   --------------------------
   -- Create_HTTP2_Context --
   --------------------------

   function Create_HTTP2_Context return Context.Object is
      Ref : constant H2CR.Element_Access := H2_Ref.Unchecked_Get;
   begin
      return (LA.Server, LA.Line, Ref.Tab_Enc'Access, Ref.Tab_Dec'Access,
              Ref.Settings'Access);
   end Create_HTTP2_Context;

   Ctx : constant Context.Object := Create_HTTP2_Context;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
   begin
      AWS.Status.Set.Free (Request);

      for T of S loop
         AWS.Status.Set.Free (T.Status.all);
      end loop;
   end Finalize;

   -----------------
   -- Handle_Body --
   -----------------

   procedure Handle_Body (Stream : HTTP2.Stream.Object) is
      Status : AWS.Status.Data renames Stream.Status.all;

      procedure Handle_POST;
      --  Process POST message and handle possible parameters and attachments

      -----------------
      -- Handle_POST --
      -----------------

      procedure Handle_POST is
         use Ada.Strings.Unbounded;

         procedure Named_Value
           (Name, Value : String; Quit : in out Boolean);
         --  Looking for the Boundary value in the  Content-Type header line

         procedure Value (Item : String; Quit : in out Boolean);
         --  Reading the first unnamed value into the Status_Content_Type
         --  variable from the Content-Type header line.

         function Get_Line return String;
         --  Read a line from Sock

         procedure Read (Buffer : out Stream_Element_Array);
         --  Fill buffer from Sock

         procedure Read_Body
           (Stat : in out AWS.Status.Data; Boundary : String);
         --  Read Sock until Boundary is found

         procedure Check_Data_Timeout;
         --  Check data time-out using server settings

         Status_Multipart_Boundary : Unbounded_String;
         Status_Root_Part_CID      : Unbounded_String;
         Status_Content_Type       : Unbounded_String;

         R_Body : constant not null access
                    Resources.Streams.Memory.Stream_Type'Class :=
                      AWS.Status.Binary_Data (Stream.Status.all);

         ------------------------
         -- Check_Data_Timeout --
         ------------------------

         procedure Check_Data_Timeout is
         begin
            null;
         end Check_Data_Timeout;

         --------------
         -- Get_Line --
         --------------

         function Get_Line return String is
         begin
            return R_Body.Get_Line;
         end Get_Line;

         -----------------
         -- Named_Value --
         -----------------

         procedure Named_Value
           (Name, Value : String; Quit : in out Boolean)
         is
            pragma Unreferenced (Quit);
            L_Name : constant String := Characters.Handling.To_Lower (Name);
         begin
            if L_Name = "boundary" then
               Status_Multipart_Boundary := To_Unbounded_String (Value);
            elsif L_Name = "start" then
               Status_Root_Part_CID := To_Unbounded_String (Value);
            end if;
         end Named_Value;

         ----------
         -- Read --
         ----------

         procedure Read (Buffer : out Stream_Element_Array) is
            Last : Stream_Element_Offset;
         begin
            R_Body.Read (Buffer, Last);
         end Read;

         ---------------
         -- Read_Body --
         ---------------

         procedure Read_Body
           (Stat     : in out AWS.Status.Data;
            Boundary : String)
         is
            pragma Unreferenced (Stat);
            Look_For : constant Stream_Element_Array :=
                         Translator.To_Stream_Element_Array (Boundary);
            Pos      : Stream_Element_Offset := Look_For'First;
            Buf      : Stream_Element_Array (1 .. 1);
         begin
            loop
               Read (Buf);

               if Buf (Buf'First) = Look_For (Pos) then
                  exit when Pos = Look_For'Last;
                  Pos := Pos + 1;
               else
                  Pos := Look_For'First;
               end if;
            end loop;
         end Read_Body;

         -----------------------
         -- Multipart_Message --
         -----------------------

         package Multipart_Message is new Multipart_Message_G
           (True, LA.Server.Properties,
            Get_Line, Read, Read_Body, Check_Data_Timeout);

         -----------
         -- Value --
         -----------

         procedure Value (Item : String; Quit : in out Boolean) is
         begin
            if Status_Content_Type /= Null_Unbounded_String then
               --  Only first unnamed value is the Content_Type

               Quit := True;

            elsif Item'Length > 0 then
               Status_Content_Type := To_Unbounded_String (Item);
            end if;
         end Value;

         procedure Parse is new AWS.Headers.Values.Parse (Value, Named_Value);

         S  : constant not null access AWS.Status.Data := Stream.Status;
         CT : constant String := AWS.Status.Content_Type (S.all);

         Attachments : AWS.Attachments.List;

      begin
         --  Parse Content-Type to get the multipart information

         Parse (CT);

         R_Body.Reset;

         if CT = MIME.Application_Form_Data then
            AWS.Status.Set.Parameters_From_Body (S.all);

         elsif Status_Content_Type = MIME.Multipart_Form_Data then
            --  This is a file upload

            Multipart_Message.File_Upload
              (S.all,
               Attachments,
               "--" & To_String (Status_Multipart_Boundary),
               "--" & To_String (Status_Multipart_Boundary) & "--",
               True);

         elsif Status_Content_Type = MIME.Multipart_Related then
            --  Attachments are to be written to separate files

            Multipart_Message.Store_Attachments
              (S.all,
               Attachments,
               "--" & To_String (Status_Multipart_Boundary),
               "--" & To_String (Status_Multipart_Boundary) & "--",
               True,
               To_String (Status_Multipart_Boundary),
               To_String (Status_Root_Part_CID));
         end if;

         AWS.Status.Reset_Body_Index (S.all);

         AWS.Status.Set.Uploaded (S.all);

         --  Record attachments into status data

         AWS.Status.Set.Attachments (LA.Stat.all, Attachments);
      end Handle_POST;

      use type AWS.Status.Request_Method;

   begin
      Stream.Append_Body (Status);

      --  With HTTP/2 the whole body is always uploaded when receiving frames

      AWS.Status.Set.Uploaded (Status);

      if AWS.Status.Method (Status) = AWS.Status.POST
        and then AWS.Status.Binary_Size (Status) > 0
      then
         Handle_POST;
      end if;
   end Handle_Body;

   --------------------------
   -- Handle_Control_Frame --
   --------------------------

   procedure Handle_Control_Frame
     (Frame   : HTTP2.Frame.Object'Class;
      Error   : out HTTP2.Error_Codes)
   is
      Add_FC  : Integer;
      Streams : HTTP2.Stream.Set.Object renames S;
   begin
      Ctx.Settings.Handle_Control_Frame (Frame, Answers, Add_FC, Error);

      if Add_FC /= 0 then
         for S of Streams loop
            if HTTP2.Connection.Flow_Control_Window_Valid
                 (S.Flow_Control_Window, Add_FC)
            then
               S.Update_Flow_Control_Window (Add_FC);
            end if;
         end loop;
      end if;
   end Handle_Control_Frame;

   ---------------------------
   -- Handle_HTTP2_Settings --
   ---------------------------

   procedure Handle_HTTP2_Settings is
      HTTP2_Settings   : constant String :=
                           AWS.Headers.Get_Values
                             (AWS.Status.Header (Request),
                              Messages.HTTP2_Settings);
      Settings_Payload : constant Stream_Element_Array :=
                           Translator.Base64_Decode (HTTP2_Settings);
      Frame            : constant HTTP2.Frame.Settings.Object :=
                           HTTP2.Frame.Settings.Create (Settings_Payload);
   begin
      Ctx.Settings.Set (Frame.Values);
   end Handle_HTTP2_Settings;

   --------------------
   -- Handle_Message --
   --------------------

   function Handle_Message
     (Stream : HTTP2.Stream.Object) return AWS.HTTP2.Message.Object
   is
      Status : AWS.Status.Data renames Stream.Status.all;
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

      if Extended_Log then
         AWS.Log.Set_Field
           (LA.Server.Log, LA.Log_Data,
            "s-free-slots", Utils.Image (Free_Slots));
      end if;

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
            Response.Set.Add_Header
              (R, Characters.Handling.To_Lower (Name), Value);
         end Add_Header;

      begin
         if Stream.Upload_State = HTTP2.Stream.Upload_Oversize
           and then Response.Is_Continue (R)
         then
            return HTTP2.Message.Undefined;
         end if;

         --  The general headers

         Add_Header
           (Messages.Date_Token,
            Messages.To_HTTP_Date (Utils.GMT_Clock));

         if AWS.Response.Content_Type (R) /= "" then
            Add_Header
              (Messages.Content_Type_Token,
               AWS.Response.Content_Type (R));
         end if;

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

         return O : constant HTTP2.Message.Object :=
           HTTP2.Message.Create (R, Status, Stream.Identifier)
         do
            Stream.Response.all := R;
            AWS.Status.Set.Free (Status);
         end return;
      end;
   end Handle_Message;

   --------------------
   -- Handle_Message --
   --------------------

   procedure Handle_Message (Stream : in out HTTP2.Stream.Object) is

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

         procedure Log_Error (Message : String);
         --  Writes message to the error log and set Error to C_Protocol_Error

         ---------------
         -- Log_Error --
         ---------------

         procedure Log_Error (Message : String) is
         begin
            AWS.Log.Write
              (LA.Server.Error_Log, Stream.Status.all, Message);
            Error := HTTP2.C_Protocol_Error;
         end Log_Error;

      begin
         Error := HTTP2.C_No_Error;

         for K in 1 .. Headers.Count loop
            declare
               Header : constant String := Headers.Get_Name (K);
               Value  : constant String := Headers.Get_Value (K);
            begin
               if HTTP2.Debug then
                  Text_IO.Put_Line ("#hg " & Header & ' ' & Value);
               end if;

               if Header'Length > 1 and then Header (Header'First) = ':' then
                  --  Pseudo header

                  if Header_Found then
                     Log_Error ("pseudo headers must appear first");
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
                           Log_Error
                             ("duplicate " & Header & " pseudo header");
                           return;
                        end if;

                        Has_Pseudo (PH) := True;

                        if PH = Path and then Value = "" then
                           Log_Error ("empty header path should be rejected");
                           return;
                        end if;
                     end;

                  elsif Header /= ":authority" then
                     Log_Error ("unknown pseudo header " & Header);
                     return;
                  end if;

               else
                  Header_Found := True;
               end if;

               if Index (Header, Maps.Constants.Upper_Set) /= 0 then
                  Log_Error ("no upper case allowed");
                  return;

               elsif Header = "connection" then
                  Log_Error ("no connection specific header allowed");
                  return;

               elsif Header = "te" and then Value /= "trailers" then
                  Log_Error
                    ("no TE header except with single value ""trailers""");
                  return;
               end if;
            end;
         end loop;

         --  Check if mandatory pseudo headers are present

         for PH in Has_Pseudo'Range loop
            if not Has_Pseudo (PH) then
               Log_Error
                 ("pseudo header :"
                  & Ada.Characters.Handling.To_Lower (PH'Img)
                  & " must exists");
            end if;
         end loop;
      end Validate_Headers;

      Status   : AWS.Status.Data renames Stream.Status.all;
      Headers  : constant AWS.Headers.List := Stream.Headers;
      Error    : HTTP2.Error_Codes;
      Oversize : constant Boolean :=
                   Stream.Upload_State = HTTP2.Stream.Upload_Oversize;

   begin
      AWS.Status.Set.Reset (Status);

      AWS.Status.Set.Case_Sensitive_Parameters
        (Status, Case_Sensitive_Parameters);

      AWS.Status.Set.Headers (Status, Headers);

      --  Check headers' validity

      Validate_Headers (Headers, Error);

      if Error /= HTTP2.C_No_Error then
         raise HTTP2.Protocol_Error with
           HTTP2.Exception_Message (Error, "headers validity check fails");
      end if;

      AWS.Status.Set.Update_Data_From_Header (Status);

      --  And set the request information

      declare
         Path        : constant String := Headers.Get (Messages.Path2_Token);
         Path_Last   : Positive;
         Query_First : Positive;

      begin
         HTTP_Utils.Split_Path (Path, Path_Last, Query_First);

         AWS.Status.Set.Request
           (Status,
            Method       => Headers.Get (Messages.Method_Token),
            URI          => Path (Path'First .. Path_Last),
            HTTP_Version => HTTP_2);

         AWS.Status.Set.Query
           (Status,
            Parameters => Path (Query_First .. Path'Last));
      end;

      --  Set status socket and peername

      AWS.Status.Set.Socket (Status, Sock);

      AWS.Status.Set.Connection_Data
        (Status,
         CNF.Server_Host (LA.Server.Properties),
         AWS.Server.Status.Port (LA.Server.all),
         CNF.Security (LA.Server.Properties));

      Will_Close :=
        HTTP_Acceptors.Length (LA.Server.Acceptor) >= Keep_Alive_Close_Limit;

      AWS.Status.Set.Keep_Alive (Status, not Will_Close);

      if not Oversize then
         Handle_Body (Stream);
      end if;

      --  Set back the status caller

      LA.Stat := Stream.Status;

      declare
         M : constant HTTP2.Message.Object := Handle_Message (Stream);
      begin
         if Oversize then
            Stream.Upload_Decision (Allow => not M.Is_Defined);
         end if;

         if M.Is_Defined then
            Deferred_Messages.Append (M);
         end if;
      end;
   end Handle_Message;

   -------------------
   -- Process_Error --
   -------------------

   procedure Process_Error (E : Exception_Occurrence) is
   begin
      LA.Server.Exception_Handler
        (E,
         LA.Server.Error_Log,
         AWS.Exceptions.Data'(False, LA.Line, LA.Stat.all),
         Error_Answer);

      Log_Commit
        (Ctx.HTTP.all, Error_Answer, LA.Stat.all,
         Response.Content_Length (Error_Answer));
   end Process_Error;

   --------------------------
   -- Queue_Settings_Frame --
   --------------------------

   procedure Queue_Settings_Frame is
      use HTTP2.Frame.Settings;
   begin
      Create (To_Set (LA.Server.Properties)).Send (Sock.all);
   end Queue_Settings_Frame;

   H2C_Answer : AWS.HTTP2.Frame.List.Object;

begin
   Will_Close := False;

   --  Initialize the HPACK tables to encode/decode headers

   LA.Log_Data := AWS.Log.Empty_Fields_Table;

   --  The first bytes on the connection should be a connection preface

   if Check_Preface then
      declare
         Preface : Stream_Element_Array
                     (HTTP2.Client_Connection_Preface'Range);
      begin
         Net.Buffered.Read (Sock.all, Preface);

         if Preface /= HTTP2.Client_Connection_Preface then
            HTTP2.Frame.GoAway.Create
              (Stream_Id => 0,
               Error     => HTTP2.C_Protocol_Error).Send (Sock.all);

            raise HTTP2.Protocol_Error
              with HTTP2.Exception_Message
                     (HTTP2.C_Protocol_Error, "wrong connection preface");
         end if;
      end;
   end if;

   if HTTP2_Ref.Is_Null then
      Ctx.Settings.Set (LA.Server.Properties);

      --  Handle the settings frame now. There is two cases:
      --  1. When upgrading from HTTP/1 (h2c) handle HTTP2-Settings payload
      --     (a setting frame).
      --  2. Read the first frame which should be the settings frame if using
      --     h2.

      if AWS.Status.Protocol (Request) = AWS.Status.H2C then
         Handle_HTTP2_Settings;

      else
         --  First frame should be a setting frame

         Ctx.Settings.Set (Sock.all);
      end if;

      --  Now send AWS settings frame

      Queue_Settings_Frame;
   end if;

   --  The maximum number of simultaneous stream has now been negociated

   declare
      use HTTP2;
      use type Containers.Count_Type;
      use type HTTP2.Stream.State_Kind;

      Max_Stream    : constant Containers.Count_Type :=
                        Containers.Count_Type
                          (Ctx.Settings.Max_Concurrent_Streams);
      Stream_Opened : Containers.Count_Type := 0;
      Last_SID      : HTTP2.Stream.Id := 0;
      In_Header     : Boolean := False;
      --  Need to avoid unknown extension frame in the middle of a header block
      --  (RFC 7450, 5.5).

   begin
      --  We now need to answer to the request made during the upgrade

      if AWS.Status.Protocol (Request) = AWS.Status.H2C then
         S.Insert
           (1,
            HTTP2.Stream.Create (Sock,  1, Ctx.Settings.Initial_Window_Size));
         Stream_Opened := Stream_Opened + 1;

         S (1).Status.all := Request;

         declare
            M : HTTP2.Message.Object := Handle_Message (S (1));
         begin
            H2C_Answer := M.To_Frames (Ctx, S (1));
         end;
      end if;

      For_Every_Frame : loop
         Response.Set.Mode (Error_Answer, Response.No_Data);

         if not Deferred_Messages.Is_Empty then
            LA.Server.Slots.Mark_Phase (LA.Line, Server_Response);

            declare
               M  : HTTP2.Message.Object := Deferred_Messages.First_Element;
               SM : constant HTTP2.Stream.Set.Maps.Reference_Type :=
                      S.Reference (M.Stream_Id);
            begin
               if (SM.Flow_Control_Window > 0
                   and then Ctx.Settings.Flow_Control_Window > 0)
                 or else not M.Has_Body
               then
                  Deferred_Messages.Delete_First;

                  --  Sends as much frame as possible that conform with the
                  --  current flow control window limit.

                  Answers.Append (M.To_Frames (Ctx, SM));

                  --  If some more data are available, register back the
                  --  message to send corresponding remaining data frames.

                  if M.More_Frames then
                     Deferred_Messages.Prepend (M);
                  else
                     if not Response.Keep_Alive (SM.Response.all) then
                        Will_Close := True;
                     end if;

                     Stream_Opened := Stream_Opened - 1;
                  end if;
               end if;
            end;
         end if;

         --  Send back frames if any waiting

         while not Answers.Is_Empty
           and then Net.Buffered.Pending (Sock.all) = 0
         loop
            declare
               Frame     : constant HTTP2.Frame.Object'Class :=
                             Answers.First_Element;
               Stream_Id : constant HTTP2.Stream.Id := Frame.Stream_Id;
               Stream    : access HTTP2.Stream.Object;
            begin
               if Stream_Id = 0 then
                  Frame.Send (Sock.all);

               else
                  Stream := S (Stream_Id).Element;

                  exit when Frame.Kind = K_Data
                    and then
                      Integer'Min
                        (Ctx.Settings.Flow_Control_Window,
                         S (Stream_Id).Flow_Control_Window)
                      < Integer (Frame.Length);

                  Stream.Send_Frame (Frame);

                  --  Update connection Flow Control Window

                  if Frame.Kind = K_Data then
                     Ctx.Settings.Update_Flow_Control_Window
                       (-Natural (Frame.Length));
                  end if;

                  if Frame.Has_Flag (HTTP2.Frame.End_Stream_Flag) then
                     Log_Commit
                       (Ctx.HTTP.all, Stream.Response.all,
                        Stream.Status.all, Stream.Bytes_Sent);
                  end if;
               end if;

               Answers.Delete_First;

               --  For GOAWAY frame we exclude the stream id from the list
               --  as no more data are expected to be sent.

               if Frame.Kind = K_GoAway then
                  AWS.Status.Set.Free (Stream.Status.all);
                  S.Exclude (Frame.Stream_Id);
               end if;
            end;
         end loop;

         LA.Server.Slots.Mark_Phase (LA.Line, Client_Header);

         exit For_Every_Frame when Stream_Opened = 0
           and then Deferred_Messages.Is_Empty
           and then Answers.Is_Empty
           and then (Will_Close
                     or else not Next_Request_Or_Give_Back (LA, Sock, H2_Ref));

         --  Get frame

         declare
            use type HTTP2.Error_Codes;

            Frame      : constant HTTP2.Frame.Object'Class :=
                           HTTP2.Frame.Read (Sock.all, Ctx.Settings.all);
            Stream_Id  : constant HTTP2.Stream_Id := Frame.Stream_Id;
            Prev_State : Stream.State_Kind;
            Error      : Error_Codes;

            procedure Go_Away (Error : HTTP2.Error_Codes; Message : String);
            --  Send GoAway frame, log message to error log and set Will_Close
            --  flag to True.

            -------------
            -- Go_Away --
            -------------

            procedure Go_Away (Error : HTTP2.Error_Codes; Message : String) is

               function Get_Status return access AWS.Status.Data;

               ----------------
               -- Get_Status --
               ----------------

               function Get_Status return access AWS.Status.Data is
                  package SM renames HTTP2.Stream.Set.Maps;
                  CS : constant SM.Cursor :=
                         (if Stream_Id = 0
                          then SM.No_Element
                          else S.Find (Stream_Id));
               begin
                  if SM.Has_Element (CS) then
                     return S (CS).Status;
                  else
                     return LA.Stat;
                  end if;
               end Get_Status;

            begin
               if Message /= "" then
                  AWS.Log.Write
                    (LA.Server.Error_Log, Get_Status.all, Message);
               end if;

               HTTP2.Frame.GoAway.Create
                 (Stream_Id => Last_SID, Error => Error).Send (Sock.all);

               Will_Close := True;
            end Go_Away;

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

            elsif not Frame.Is_Valid (Ctx.Settings.all, Error) then
               --  Send a GOAWAY response right now

               Go_Away (Error, "invalid frame " & Frame.Kind'Img);

               exit For_Every_Frame;

            elsif Frame.Kind = K_GoAway then
               --  if a GOAWAY frame is received we need to exit now

               Will_Close := True;
               exit For_Every_Frame;

            elsif Stream_Id = 0 then
               Handle_Control_Frame (Frame, Error);

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
                    or else Stream_Opened >= Max_Stream
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
                          (Sock, Stream_Id, Ctx.Settings.Initial_Window_Size));
                     Stream_Opened := Stream_Opened + 1;
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
                  else
                     Go_Away
                       (Error,
                        Error'Img & " from Received_Frame "
                        & S (Stream_Id).Error_Detail'Img);
                  end if;

                  exit For_Every_Frame;
               end if;

               if S (Stream_Id).Is_Message_Ready
                 and then Prev_State < Stream.Half_Closed_Remote
               then
                  LA.Server.Slots.Mark_Phase (LA.Line, Server_Processing);

                  if S (Stream_Id).Upload_State = Stream.Upload_Accepted then
                     Handle_Body (S (Stream_Id));
                     Deferred_Messages.Append (Handle_Message (S (Stream_Id)));

                  elsif S (Stream_Id).Upload_State /= Stream.Upload_Rejected
                  then
                     Handle_Message (S (Stream_Id));
                  end if;

               elsif S (Stream_Id).Upload_State = Stream.Upload_Oversize then
                  LA.Server.Slots.Mark_Phase (LA.Line, Server_Processing);
                  Handle_Message (S (Stream_Id));
               end if;
            end if;
         end;

         if not H2C_Answer.Is_Empty then
            Answers.Append (H2C_Answer);
            H2C_Answer.Clear;
         end if;
      end loop For_Every_Frame;
   end;

exception
   when E : Net.Socket_Error =>
      Will_Close := True;

      if HTTP2.Debug then
         Ada.Text_IO.Put_Line ("#ex " & Exception_Message (E));
      end if;

   when E : HTTP2.Protocol_Error =>
      Will_Close := True;

      AWS.Log.Write
        (LA.Server.Error_Log,
         LA.Stat.all,
         Utils.CRLF_2_Spaces (Exception_Information (E)));

      Process_Error (E);

   when E : Net.Buffered.Data_Overflow
      | Parameters.Too_Long_Parameter
      | Parameters.Too_Many_Parameters
      =>

      if HTTP2.Debug then
         Ada.Text_IO.Put_Line ("#ex " & Exception_Message (E));
      end if;

      Will_Close := True;

      HTTP2.Frame.GoAway.Create
        (Stream_Id => 1,
         Error     => AWS.HTTP2.C_Refused_Stream,
         Code      => (if Exception_Identity (E) =
                         Parameters.Too_Many_Parameters'Identity
                       then Messages.S403
                       else Messages.S400),
         Message   => Exception_Message (E)).Send (Sock.all);

      Process_Error (E);

   when E : others =>
      Will_Close := True;

      AWS.Log.Write
        (LA.Server.Error_Log,
         LA.Stat.all,
         Utils.CRLF_2_Spaces (Exception_Information (E)));

      Process_Error (E);

      LA.Server.Slots.Mark_Phase (LA.Line, Server_Response);

      HTTP2.Frame.GoAway.Create
        (Stream_Id => 1,
         Error     => AWS.HTTP2.C_Internal_Error,
         Code      => Messages.S500,
         Message   => Response.Message_Body (Error_Answer)).Send (Sock.all);
end Protocol_Handler_V2;
