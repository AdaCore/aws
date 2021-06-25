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
with Ada.Exceptions;
with Ada.Streams;
with Ada.Strings.Unbounded;

with AWS.Headers;
with AWS.HTTP2.Connection;
with AWS.HTTP2.Frame.Continuation;
with AWS.HTTP2.Frame.Data;
with AWS.HTTP2.Frame.Headers;
with AWS.HTTP2.Frame.Window_Update;
with AWS.HTTP2.HPACK;

package body AWS.HTTP2.Stream is

   use Ada.Exceptions;
   use Ada.Strings.Unbounded;

   use all type HTTP2.Frame.Kind_Type;

   ------------
   -- Create --
   ------------

   function Create
     (Sock        : not null Net.Socket_Access;
      Identifier  : Id;
      Window_Size : Natural;
      Weight      : Byte_1 := Frame.Priority.Default_Weight) return Object is
   begin
      return Object'
        (Sock                => Sock,
         Id                  => Identifier,
         State               => Idle,
         Frames              => Frame.List.Empty_List,
         Is_Ready            => False,
         Header_Found        => False,
         Flow_Control_Window => Window_Size,
         Weight              => Weight,
         Stream_Dependency   => 0);
   end Create;

   ----------------------
   -- Is_Message_Ready --
   ----------------------

   function Is_Message_Ready (Self : Object) return Boolean is
   begin
      return Self.Is_Ready;
   end Is_Message_Ready;

   -------------
   -- Message --
   -------------

   function Message
     (Self : Object;
      Ctx  : in out Server.Context.Object) return HTTP2.Message.Object
   is
      use Ada.Streams;

      L : Frame.List.Object;
      --  Record all frames for a full header definition (last frame must have
      --  the end of hedaer flag set).

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
                  raise Protocol_Error with "header with length of 0";
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
         return Get_Headers (Ctx.Table, Ctx.Settings);
      exception
         when E : others =>
            raise Protocol_Error with Exception_Message (E);
      end Parse_Header;

      H       : Headers.List;
      Payload : Unbounded_String;

   begin
      for F of Self.Frames loop
         case F.Kind is
            when K_Headers =>
               L.Append (F);

               if F.Has_Flag (Frame.End_Headers_Flag) then
                  H := H.Union (Parse_Header, Unique => True);
               end if;

            when K_Continuation =>
               L.Append (F);

               if F.Has_Flag (Frame.End_Headers_Flag) then
                  H := H.Union (Parse_Header, Unique => True);
               end if;

            when K_Data =>
               Payload := Frame.Data.Object (F).Payload;

            when others =>
               raise Constraint_Error with
                 "wrong frame kind registered in message object";
         end case;
      end loop;

      return HTTP2.Message.Create (H, Payload, Self.Id);
   end Message;

   --------------------
   -- Received_Frame --
   --------------------

   procedure Received_Frame
     (Self  : in out Object;
      Frame : HTTP2.Frame.Object'Class;
      Error : out Error_Codes)
   is
      use type Ada.Containers.Count_Type;

      procedure Handle_Priority (Priority : HTTP2.Frame.Priority.Payload);
      --  Handle priority information

      procedure Handle_Window_Update
        (Frame : HTTP2.Frame.Window_Update.Object);
      --  Handle frame window upade, record corresponding information in the
      --  frame.

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
         if Connection.Flow_Control_Window_Valid
           (Self.Flow_Control_Window, Incr)
         then
            Self.Flow_Control_Window := Self.Flow_Control_Window + Incr;
         else
            Error := HTTP2.C_Flow_Control_Error;
            return;
         end if;
      end Handle_Window_Update;

      Has_Prev_Frame : constant Boolean := Self.Frames.Length > 0;

      End_Header     : constant Boolean :=
                         (Frame.Has_Flag (HTTP2.Frame.End_Headers_Flag));
      End_Stream     : constant Boolean :=
                         (Frame.Has_Flag (HTTP2.Frame.End_Stream_Flag));

   begin
      Error := C_No_Error;

      --  A received frame must have an odd number

      if Self.Id /= 0 and then Self.Id mod 2 = 0 then
         Error := C_Protocol_Error;
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
                  else
                     null;
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
                  return;
            end case;

         when Half_Closed_Local =>
            case Frame.Kind is
               when K_RST_Stream =>
                  Self.State := Closed;
               when others =>
                  if End_Stream then
                     Error := C_Protocol_Error;
                     return;
                  else
                     null;
                  end if;
            end case;

         when Half_Closed_Remote =>
            case Frame.Kind is
               when K_RST_Stream =>
                  Self.State := Closed;
               when K_Data | K_Headers | K_Continuation =>
                  Error := C_Stream_Closed;
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
                  return;
            end case;
      end case;

      --  Handle frame's content if needed

      if Has_Prev_Frame
        and then Self.Frames.Last_Element.Kind = K_Continuation
        and then Frame.Kind /= K_Continuation
      then
         Error := C_Protocol_Error;
         return;
      end if;

      case Frame.Kind is
         when K_Priority =>
            if Has_Prev_Frame
              and then not Self.Frames.Last_Element.Has_Flag
                             (HTTP2.Frame.End_Headers_Flag)
              and then Self.Frames.Last_Element.Kind
                         in K_Headers | K_Continuation
            then
               Error := C_Protocol_Error;
               return;
            end if;

            Handle_Priority (HTTP2.Frame.Priority.Object (Frame).Get_Payload);

         when K_Window_Update =>
            if Has_Prev_Frame
              and then not Self.Frames.Last_Element.Has_Flag
                             (HTTP2.Frame.End_Headers_Flag)
              and then Self.Frames.Last_Element.Kind
                         in K_Headers | K_Continuation
            then
               Error := C_Protocol_Error;
               return;
            end if;

            Handle_Window_Update (HTTP2.Frame.Window_Update.Object (Frame));

         when K_Headers | K_Data =>
            if Frame.Kind = K_Headers then
               --  An header has already been sent, and so this header must
               --  have a end-strem flag.

               if not Frame.Has_Flag (HTTP2.Frame.End_Stream_Flag)
                 and then Self.Header_Found
               then
                  Error := C_Protocol_Error;
                  return;

               else
                  Self.Header_Found := True;
               end if;
            end if;

            Self.Frames.Append (Frame);

            --  An header frame can have a priority chunk, handle it now

            if Frame.Kind = K_Headers
              and then Frame.Has_Flag (HTTP2.Frame.Priority_Flag)
            then
               Handle_Priority
                 (HTTP2.Frame.Headers.Object (Frame).Get_Priority);
            end if;

            --  For a message to be ready we at least need the stream to be in
            --  open state (i.e. it has received an header frame to open it).

            Self.Is_Ready := Self.State /= Idle
              and then (Frame.Kind = K_Data
                        or else (Frame.Kind = K_Headers and then End_Header));

         when K_Continuation =>
            --  A continuation frame is only valid if following an header,
            --  push_promise or continuation frame.

            if Has_Prev_Frame
              and then Self.Frames.Last_Element.Kind
                         not in K_Headers | K_Continuation | K_Push_Promise
            then
               Error := C_Protocol_Error;
               return;
            end if;

            Self.Frames.Append (Frame);

            Self.Is_Ready := Self.State /= Idle
              and then End_Header;

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
                  raise Protocol_Error;
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
                  raise Protocol_Error;
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
         Self.Flow_Control_Window :=
           Self.Flow_Control_Window - Natural (Frame.Length);
      end if;
   end Send_Frame;

   --------------------------------
   -- Update_Flow_Control_Window --
   --------------------------------

   procedure Update_Flow_Control_Window
     (Self      : in out Object;
      Increment : Integer) is
   begin
      Self.Flow_Control_Window := Self.Flow_Control_Window + Increment;
   end Update_Flow_Control_Window;

end AWS.HTTP2.Stream;
