------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                      Copyright (C) 2021, AdaCore                         --
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

with Ada.Containers;

with AWS.Default;
with AWS.HTTP2.Connection;
with AWS.HTTP2.Frame.Continuation;
with AWS.HTTP2.Frame.Data;
with AWS.HTTP2.Frame.Headers;
with AWS.HTTP2.Frame.Window_Update;
with AWS.HTTP2.HPACK;
with AWS.Server.Context;
with AWS.Messages;
with AWS.Response.Set;
with AWS.Status.Set;

package body AWS.HTTP2.Stream is

   use all type HTTP2.Frame.Kind_Type;

   procedure Parse_Headers
     (Self : in out Object; Ctx : Server.Context.Object);

   -----------------
   -- Append_Body --
   -----------------

   procedure Append_Body (Self : Object; Status : in out AWS.Status.Data) is
   begin
      for F of Self.D_Frames loop
         pragma Assert (F.Kind = K_Data);

         Frame.Data.Object (F).Append (Status);
      end loop;

      AWS.Status.Set.Uploaded (Status);
   end Append_Body;

   procedure Append_Body
     (Self     : Object;
      Response : in out AWS.Response.Data) is
   begin
      if not Self.D_Frames.Is_Empty then
         AWS.Response.Set.Mode (Response, AWS.Response.Message);

         for F of Self.D_Frames loop
            pragma Assert (F.Kind = K_Data);

            Frame.Data.Object (F).Append (Response);
         end loop;
      end if;
   end Append_Body;

   ------------
   -- Create --
   ------------

   function Create
     (Sock        : not null Net.Socket_Access;
      Identifier  : Id;
      Window_Size : Natural;
      Weight      : Byte_1 := Frame.Priority.Default_Weight) return Object is
   begin
      return Self : Object := Object'
        (Sock                => Sock,
         Id                  => Identifier,
         State               => Idle,
         H_Frames            => Frame.List.Empty_List,
         D_Frames            => Frame.List.Empty_List,
         Headers             => AWS.Headers.Empty_List,
         Status              => <>,
         Response            => <>,
         Is_Ready            => False,
         Header_Found        => False,
         Upload_State        => Upload_Idle,
         Flow_Send_Window    => Window_Size,
         Flow_Receive_Window => Default.HTTP2_Initial_Window_Size,
         Bytes_Sent          => 0,
         Weight              => Weight,
         Stream_Dependency   => 0,
         End_Stream          => False,
         Content_Length      => Undefined_Length,
         Bytes_Received      => 0,
         Data_Flow           => Unknown,
         Error_Detail        => <>)
      do
         Self.Headers.Case_Sensitive (False);
      end return;
   end Create;

   ----------------------
   -- Is_Message_Ready --
   ----------------------

   function Is_Message_Ready (Self : Object) return Boolean is
   begin
      return Self.Is_Ready;
   end Is_Message_Ready;

   -------------------
   -- Parse_Headers --
   -------------------

   procedure Parse_Headers
     (Self : in out Object; Ctx : Server.Context.Object)
   is
      L : Frame.List.Object renames Self.H_Frames;
      --  Complete header block (last frame must have the end of header flag
      --  set).

      function Parse_Header return AWS.Headers.List;
      --  Parse headers & continuation frames

      ------------------
      -- Parse_Header --
      ------------------

      function Parse_Header return AWS.Headers.List is

         I      : Stream_Element_Offset := 1; -- Current header data index
         EOH    : Boolean := False;           -- End Of Headers
         Length : Stream_Element_Count := 0;  -- Current header content length

         function End_Of_Stream return Boolean;

         function Next return Stream_Element;
         --  Get next element from current frame

         function End_Of_Stream return Boolean is (EOH);
         --  Returns true if end of headers found

         procedure Next_Frame with Inline;
         --  Move to next frame if available, set EOS otherwise

         ----------
         -- Next --
         ----------

         function Next return Stream_Element is
            F : constant Frame.Object'Class := L.First_Element;
            E : Stream_Element;
         begin
            if Length = 0 then
               if F.Kind = K_Headers then
                  Length := Frame.Headers.Object (F).Content_Length;
               else
                  Length := Frame.Continuation.Object (F).Content_Length;
               end if;

               if Length = 0 then
                  raise Protocol_Error with
                    Exception_Message
                      (C_Protocol_Error, "header with length of 0");
               end if;
            end if;

            if F.Kind = K_Headers then
               E := Frame.Headers.Object (F).Get (I);
            else
               E := Frame.Continuation.Object (F).Get (I);
            end if;

            I := I + 1;

            if I > Length then
               Next_Frame;
            end if;

            return E;
         end Next;

         ----------------
         -- Next_Frame --
         ----------------

         procedure Next_Frame is
         begin
            L.Delete_First;

            if L.Is_Empty then
               EOH := True;
            else
               Length := 0;
            end if;

            I := 1;
         end Next_Frame;

         function Get_Headers is new AWS.HTTP2.HPACK.Decode
           (End_Of_Stream => End_Of_Stream,
            Get_Byte      => Next);

      begin
         return Get_Headers (Ctx.Tab_Dec, Ctx.Settings);
      end Parse_Header;

   begin
      Self.Headers.Union (Parse_Header, Unique => False);

      declare
         Content_Length : constant String :=
                            Self.Headers.Get (Messages.Content_Length_Token);
      begin
         if Content_Length /= "" then
            Self.Content_Length := Content_Length_Type'Value (Content_Length);

            if Ctx.HTTP /= null
              and then Self.Content_Length >
                Content_Length_Type
                  (Server.Config (Ctx.HTTP.all).Upload_Size_Limit)
            then
               Self.Upload_State := Upload_Oversize;
            end if;
         end if;
      end;
   end Parse_Headers;

   --------------------
   -- Received_Frame --
   --------------------

   procedure Received_Frame
     (Self  : in out Object;
      Ctx   : Server.Context.Object;
      Frame : HTTP2.Frame.Object'Class;
      Error : out Error_Codes)
   is
      use type Ada.Containers.Count_Type;

      Info : Error_Details renames Self.Error_Detail;

      procedure Handle_Priority (Priority : HTTP2.Frame.Priority.Payload);
      --  Handle priority information

      procedure Handle_Window_Update
        (Frame : HTTP2.Frame.Window_Update.Object);
      --  Handle frame window upade, record corresponding information in the
      --  stream and connection.

      ---------------------
      -- Handle_Priority --
      ---------------------

      procedure Handle_Priority (Priority : HTTP2.Frame.Priority.Payload) is
      begin
         Self.Weight := Priority.Weight;
         Self.Stream_Dependency := Priority.Stream_Dependency;
      end Handle_Priority;

      --------------------------
      -- Handle_Window_Update --
      --------------------------

      procedure Handle_Window_Update
        (Frame : HTTP2.Frame.Window_Update.Object)
      is
         Incr : constant Natural := Natural (Frame.Size_Increment);
      begin
         if Connection.Flow_Control_Window_Valid (Self.Flow_Send_Window, Incr)
         then
            Self.Flow_Send_Window := Self.Flow_Send_Window + Incr;
         else
            Error := HTTP2.C_Flow_Control_Error;
         end if;
      end Handle_Window_Update;

      Has_Prev_Frame : constant Boolean := Self.H_Frames.Length > 0;

      End_Header     : constant Boolean :=
                         (Frame.Has_Flag (HTTP2.Frame.End_Headers_Flag));
      End_Stream     : constant Boolean :=
                         (Frame.Has_Flag (HTTP2.Frame.End_Stream_Flag));

   begin
      Self.Data_Flow := Receiving;

      Error := C_No_Error;
      Info  := Error_No_Details;

      --  A received frame must have an odd number

      if Self.Id /= 0 and then Self.Id mod 2 = 0 then
         Error := C_Protocol_Error;
         Info  := Error_Stream_Id_Even;
         return;
      end if;

      --  Handle Stream States - See RFC-7540 5.1

      case Self.State is
         when Idle =>
            case Frame.Kind is
               when K_Headers =>
                  Self.State := (if End_Stream and End_Header
                                 then Half_Closed_Remote
                                 else Open);
               when K_Push_Promise =>
                  Self.State := Reserved_Remote;
               when K_Priority =>
                  null;
               when others =>
                  Error := C_Protocol_Error;
                  Info  := Error_From_Idle;
                  return;
            end case;

         when Open =>
            case Frame.Kind is
               when K_RST_Stream =>
                  Self.State := Closed;
               when K_Continuation =>
                  if End_Header then
                     Self.State := Half_Closed_Remote;
                  end if;
               when others =>
                  --  All other frames are ok in the open state
                  if End_Stream then
                     Self.State := Half_Closed_Remote;
                  end if;
            end case;

         when Reserved_Local =>
            case Frame.Kind is
               when K_RST_Stream =>
                  Self.State := Closed;
               when others =>
                  if End_Stream then
                     Self.State := Half_Closed_Remote;
                  end if;
            end case;

         when Reserved_Remote =>
            case Frame.Kind is
               when K_RST_Stream =>
                  Self.State := Closed;
               when K_Priority | K_Headers =>
                  null;
               when others =>
                  Error := C_Protocol_Error;
                  Info  := Error_Reserved_Remote;
                  return;
            end case;

         when Half_Closed_Local =>
            case Frame.Kind is
               when K_RST_Stream =>
                  Self.State := Closed;
               when others =>
                  if End_Stream then
                     if Self.Upload_State /= Upload_Rejected then
                        Error := C_Protocol_Error;
                        Info  := Error_Half_Closed_Local;
                        return;
                     end if;

                     Self.State := Closed;
                  end if;
            end case;

         when Half_Closed_Remote =>
            case Frame.Kind is
               when K_RST_Stream =>
                  Self.State := Closed;
               when K_Data | K_Headers | K_Continuation =>
                  Error := C_Stream_Closed;
                  Info  := Error_Half_Closed_Remote;
                  return;
               when others =>
                  null;
            end case;

         when Closed =>
            case Frame.Kind is
               when K_Priority | K_RST_Stream =>
                  null;
               when K_Window_Update =>
                  --  Allowed for a short period of time
                  null;
               when others =>
                  Error := C_Stream_Closed;
                  Info  := Error_Closed;
                  return;
            end case;
      end case;

      --  Handle frame's content if needed

      if Has_Prev_Frame
        and then Self.H_Frames.Last_Element.Kind = K_Continuation
        and then Frame.Kind /= K_Continuation
      then
         Error := C_Protocol_Error;
         Info  := Error_No_Continuation;
         return;
      end if;

      case Frame.Kind is
         when K_Priority =>
            if Has_Prev_Frame
              and then not Self.H_Frames.Last_Element.Has_Flag
                             (HTTP2.Frame.End_Headers_Flag)
            then
               Error := C_Protocol_Error;
               Info  := Error_Priority;
               return;
            end if;

            Handle_Priority (HTTP2.Frame.Priority.Object (Frame).Get_Payload);

         when K_Window_Update =>
            if Has_Prev_Frame
              and then not Self.H_Frames.Last_Element.Has_Flag
                             (HTTP2.Frame.End_Headers_Flag)
            then
               Error := C_Protocol_Error;
               Info  := Error_Window_Update;
               return;
            end if;

            Handle_Window_Update (HTTP2.Frame.Window_Update.Object (Frame));

         when K_Headers =>
            if End_Stream then
               --  End_Stream flag in header frame spread it to last
               --  continuation frame in the header block.

               Self.End_Stream := True;
            end if;

            --  An header has already been sent, and so this header must
            --  have an end-stream flag.

            if not End_Stream and then Self.Header_Found then
               Error := C_Protocol_Error;
               Info  := Error_Headers;
               return;

            else
               Self.Header_Found := True;
            end if;

            Self.H_Frames.Append (Frame);

            --  An header frame can have a priority chunk, handle it now

            if Frame.Has_Flag (HTTP2.Frame.Priority_Flag) then
               Handle_Priority
                 (HTTP2.Frame.Headers.Object (Frame).Get_Priority);
            end if;

            if End_Header then
               Self.Parse_Headers (Ctx);
            end if;

            --  For a message to be ready we at least need the stream to be in
            --  open state (i.e. it has received an header frame to open it).

            Self.Is_Ready := Self.State /= Idle
              and then End_Header and then End_Stream;

         when K_Continuation =>
            --  A continuation frame is only valid if following an header,
            --  push_promise or continuation frame.

            if not Has_Prev_Frame then
               Error := C_Protocol_Error;
               Info  := Error_Continuation;
               return;
            end if;

            Self.H_Frames.Append (Frame);

            if End_Header then
               Self.Parse_Headers (Ctx);
            end if;

            Self.Is_Ready := Self.State /= Idle
              and then End_Header
              and then (Self.End_Stream or else End_Stream);

         when K_Data =>
            Self.D_Frames.Append (Frame);

            declare
               package WU renames AWS.HTTP2.Frame.Window_Update;
               PL : constant Positive :=
                      HTTP2.Frame.Data.Object (Frame).Payload_Length;
            begin
               Self.Bytes_Received := Self.Bytes_Received +
                 Content_Length_Type (PL);
               Self.Flow_Receive_Window := Self.Flow_Receive_Window - PL;
               Ctx.Settings.Update_Flow_Receive_Window (-PL);

               if Self.Flow_Receive_Window
                 < Positive (Ctx.Settings.Max_Frame_Size)
                 and then Self.Upload_State /= Upload_Rejected
               then
                  WU.Create
                    (Self.Id,
                     WU.Size_Increment_Type'Last
                     - WU.Size_Increment_Type (Self.Flow_Receive_Window))
                      .Send (Self.Sock.all);

                  Self.Flow_Receive_Window :=
                    Positive (WU.Size_Increment_Type'Last);
               end if;

               if Ctx.Settings.Flow_Receive_Window
                 < Positive (Ctx.Settings.Max_Frame_Size)
               then
                  declare
                     Increment : constant WU.Size_Increment_Type :=
                                   WU.Size_Increment_Type'Last -
                                     WU.Size_Increment_Type
                                       (Ctx.Settings.Flow_Receive_Window);
                  begin
                     WU.Create (0, Increment).Send (Self.Sock.all);

                     Ctx.Settings.Update_Flow_Receive_Window
                       (Positive (Increment));
                  end;
               end if;
            end;

            if Self.Content_Length /= Undefined_Length
              and then
                (if End_Stream
                 then Self.Bytes_Received /= Self.Content_Length
                 else Self.Bytes_Received > Self.Content_Length)
            then
               Error := C_Protocol_Error;
               Info  := Error_Content_Length;
               return;
            end if;

            Self.Is_Ready := Self.State /= Idle and then End_Stream;

         when others =>
            null;
      end case;
   end Received_Frame;

   ----------------
   -- Send_Frame --
   ----------------

   procedure Send_Frame
     (Self  : in out Object;
      Frame : HTTP2.Frame.Object'Class)
   is
      End_Header : constant Boolean :=
                     (Frame.Has_Flag (HTTP2.Frame.End_Headers_Flag));
      End_Stream : constant Boolean :=
                     Frame.Has_Flag (HTTP2.Frame.End_Stream_Flag);
   begin
      Self.Data_Flow := Sending;

      Frame.Send (Self.Sock.all);

      --  Handle Stream States - See RFC-7540 5.1

      case Self.State is
         when Idle =>
            case Frame.Kind is
               when K_Headers =>
                  Self.State := (if End_Stream and End_Header
                                 then Half_Closed_Remote
                                 else Open);
               when K_Push_Promise =>
                  Self.State := Reserved_Local;
               when K_Priority =>
                  null;
               when others =>
                  raise Protocol_Error with
                    Exception_Message (C_Protocol_Error, "stream state error");
            end case;

         when Open =>
            case Frame.Kind is
               when K_RST_Stream =>
                  Self.State := Closed;
               when K_Continuation =>
                  if End_Header then
                     Self.State := Half_Closed_Local;
                  end if;
               when others =>
                  --  All other frames are ok in the open state

                  if End_Stream then
                     Self.State := Half_Closed_Local;
                  end if;
            end case;

         when Reserved_Local =>
            case Frame.Kind is
               when K_RST_Stream =>
                  Self.State := Closed;
               when K_Headers =>
                  Self.State := Half_Closed_Remote;
               when others =>
                  null;
            end case;

         when Reserved_Remote =>
            case Frame.Kind is
               when K_RST_Stream =>
                  Self.State := Closed;
               when K_Priority | K_Headers =>
                  null;
               when others =>
                  raise Protocol_Error with
                    Exception_Message (C_Protocol_Error, "stream state error");
            end case;

         when Half_Closed_Local =>
            case Frame.Kind is
               when K_RST_Stream =>
                  Self.State := Closed;
               when others =>
                  null;
            end case;

         when Half_Closed_Remote =>
            case Frame.Kind is
               when K_RST_Stream =>
                  Self.State := Closed;
               when others =>
                  if End_Stream then
                     Self.State := Closed;
                  else
                     null;
                  end if;
            end case;

         when Closed =>
            case Frame.Kind is
               when K_Priority =>
                  null;
               when others =>
                  null;
            end case;
      end case;

      --  Update stream's Flow Control Window

      if Frame.Kind = K_Data then
         Self.Flow_Send_Window :=
           Self.Flow_Send_Window - Natural (Frame.Length);
         Self.Bytes_Sent :=
           Self.Bytes_Sent + Stream_Element_Count (Frame.Length);
      end if;
   end Send_Frame;

   --------------------------------
   -- Update_Flow_Control_Window --
   --------------------------------

   procedure Update_Flow_Control_Window
     (Self      : in out Object;
      Increment : Integer) is
   begin
      Self.Flow_Send_Window := Self.Flow_Send_Window + Increment;
   end Update_Flow_Control_Window;

   ---------------------
   -- Upload_Decision --
   ---------------------

   procedure Upload_Decision (Self : in out Object; Allow : Boolean) is
   begin
      Self.Upload_State := (if Allow
                            then Upload_Accepted
                            else Upload_Rejected);
   end Upload_Decision;

end AWS.HTTP2.Stream;
