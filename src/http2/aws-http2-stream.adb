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

with Ada.Strings.Unbounded;

with AWS.Headers;
with AWS.HTTP2.Frame.Data;
with AWS.HTTP2.Frame.Headers;
with AWS.HTTP2.Frame.Window_Update;

package body AWS.HTTP2.Stream is

   use Ada.Strings.Unbounded;

   use all type HTTP2.Frame.Kind_Type;

   ------------
   -- Create --
   ------------

   function Create
     (Sock       : not null Net.Socket_Access;
      Identifier : Id;
      Weight     : Byte_1 := Frame.Priority.Default_Weight) return Object is
   begin
      return Object'
        (Sock              => Sock,
         Id                => Identifier,
         State             => Idle,
         Frames            => Frame.List.Empty_List,
         Is_Ready          => False,
         Window_Size       => Frame.Window_Update.Default_Window_Size,
         Weight            => Weight,
         Stream_Dependency => 0);
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
      H       : Headers.List;
      Payload : Unbounded_String;
   begin
      for F of Self.Frames loop
         case F.Kind is
            when K_Headers =>
               H := H.Union
                      (Frame.Headers.Object (F).Get (Ctx.Table, Ctx.Settings),
                       Unique => True);

            when K_Data =>
               Payload := Frame.Data.Object (F).Payload;

            when others =>
               raise Constraint_Error;
         end case;
      end loop;

      return HTTP2.Message.Create (H, Payload);
   end Message;

   -------------------
   -- Receive_Frame --
   -------------------

   procedure Received_Frame
     (Self  : in out Object;
      Frame : HTTP2.Frame.Object'Class)
   is
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
        (Frame : HTTP2.Frame.Window_Update.Object) is
      begin
         Self.Window_Size := Self.Window_Size + Natural (Frame.Size_Increment);
      end Handle_Window_Update;

      End_Header : constant Boolean :=
                     (Frame.Has_Flag (HTTP2.Frame.End_Headers_Flag));
      End_Stream : constant Boolean :=
                     (Frame.Has_Flag (HTTP2.Frame.End_Stream_Flag));

   begin
      --  Handle frame's kind and state

      case Self.State is
         when Idle =>
            case Frame.Kind is
               when K_Headers =>
                  Self.State := Open;
               when K_Push_Promise =>
                  Self.State := Reserved_Remote;
               when K_Priority =>
                  null;
               when others =>
                  raise Protocol_Error;
            end case;

         when Open =>
            case Frame.Kind is
               when K_RST_Stream =>
                  Self.State := Closed;
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
                  raise Protocol_Error;
            end case;

         when Half_Closed_Local =>
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

         when Half_Closed_Remote =>
            case Frame.Kind is
               when K_RST_Stream =>
                  Self.State := Closed;
               when others =>
                  null;
            end case;

         when Closed =>
            case Frame.Kind is
               when K_Priority =>
                  null;
               when others =>
                  null;
            end case;
      end case;

      --  Handle frame's content if needed

      case Frame.Kind is
         when K_Priority =>
            Handle_Priority (HTTP2.Frame.Priority.Object (Frame).Get_Payload);

         when K_Window_Update =>
            Handle_Window_Update (HTTP2.Frame.Window_Update.Object (Frame));

         when K_Headers | K_Data =>
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
                        or else (Frame.Kind = K_Headers
                                 and then (End_Stream or else End_Header)));

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
      End_Stream : constant Boolean :=
                     Frame.Has_Flag (HTTP2.Frame.End_Stream_Flag);
   begin
      Frame.Send (Self.Sock.all);

      case Self.State is
         when Idle =>
            case Frame.Kind is
               when K_Headers =>
                  Self.State := Open;
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
   end Send_Frame;

   ---------------------
   -- Set_Window_Size --
   ---------------------

   procedure Set_Window_Size (Self : in out Object; Window_Size : Natural) is
   begin
      Self.Window_Size := Window_Size;
   end Set_Window_Size;

end AWS.HTTP2.Stream;
