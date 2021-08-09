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

with Ada.Calendar;
with Ada.Streams;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.HTTP2.Connection;
with AWS.HTTP2.Frame.Continuation;
with AWS.HTTP2.Frame.Data;
with AWS.HTTP2.Frame.Headers;
with AWS.HTTP2.Stream;
with AWS.Messages;
with AWS.Resources;
with AWS.Server.HTTP_Utils;

package body AWS.HTTP2.Message is

   use Ada.Streams;
   use Ada.Strings.Unbounded;

   ------------
   -- Create --
   ------------

   function Create
     (Answer    : in out Response.Data;
      Request   : AWS.Status.Data;
      Stream_Id : HTTP2.Stream_Id) return Object
   is
      O : Object;

      procedure Set_Body;

      --------------
      -- Set_Body --
      --------------

      procedure Set_Body is
         Size : Stream_Element_Offset;
      begin
         O.M_Body := Response.Create_Stream
           (Answer, AWS.Status.Is_Supported (Request, Messages.GZip));

         Size := O.M_Body.Size;

         if Size /= Resources.Undefined_Length then
            O.Headers.Add (Messages.Content_Length_Token, Utils.Image (Size));
         end if;
      end Set_Body;

   begin
      O.Mode      := Response.Mode (Answer);
      O.Stream_Id := Stream_Id;

      case O.Mode is
         when Response.Message | Response.Header =>
            --  Set status code

            O.Headers.Add
              (Messages.Status_Token,
               Messages.Image (Response.Status_Code (Answer)));

            if O.Mode /= Response.Header then
               Set_Body;
            end if;

         when Response.File | Response.File_Once | Response.Stream =>

            declare
               use all type Server.HTTP_Utils.Resource_Status;
               use type Status.Request_Method;
               use type Ada.Calendar.Time;

               File_Time : Ada.Calendar.Time;
               F_Status  : constant Server.HTTP_Utils.Resource_Status :=
                             Server.HTTP_Utils.Get_Resource_Status
                               (Request,
                                Response.Filename (Answer),
                                File_Time);

               Status_Code : Messages.Status_Code :=
                               Response.Status_Code (Answer);
               With_Body   : constant Boolean :=
                               Messages.With_Body (Status_Code)
                                 and then Status.Method (Request)
                                 /= Status.HEAD;
            begin
               --  Status code header

               case F_Status is
                  when Changed    =>
                     if AWS.Headers.Get_Values
                       (Status.Header (Request), Messages.Range_Token) /= ""
                       and then With_Body
                     then
                        Status_Code := Messages.S200;
                     end if;

                  when Up_To_Date =>
                     Status_Code := Messages.S304;

                  when Not_Found  =>
                     Status_Code := Messages.S404;
               end case;

               O.Headers.Add
                 (Messages.Status_Token, Messages.Image (Status_Code));

               if File_Time /= Utils.AWS_Epoch
                 and then not Response.Has_Header
                                (Answer, Messages.Last_Modified_Token)
               then
                  O.Headers.Add
                    (Messages.Last_Modified_Token,
                     Messages.To_HTTP_Date (File_Time));
               end if;

               if With_Body then
                  Set_Body;
               end if;
            end;

         when Response.WebSocket =>
            raise Constraint_Error with "websocket is HTTP/1.1 only";

         when Response.Socket_Taken =>
            raise Constraint_Error with "not yet supported";

         when Response.No_Data =>
            raise Constraint_Error with "no_data should never happen";
      end case;

      O.Headers.Union (Response.Header (Answer), False);

      return O;
   end Create;

   ---------------
   -- To_Frames --
   ---------------

   function To_Frames
     (Self   : in out Object;
      Ctx    : in out Server.Context.Object;
      Stream : HTTP2.Stream.Object)
      return AWS.HTTP2.Frame.List.Object
   is
      FCW  : Integer :=
               Integer'Min
                 (Stream.Flow_Control_Window,
                  Ctx.Settings.Flow_Control_Window);
      --  Current flow control window, the corresponds to the max frame data
      --  content that will be sent. That is, the returns list will not exeed
      --  this value, the remaining frames will be created during a second call
      --  if More_Frames returns True.

      List : Frame.List.Object;
      --  The list of created frames

      procedure Handle_Headers (Headers : AWS.Headers.List);
      --  Create the header frames

      procedure From_Stream;
      --  Creates the data frame Self.Stream

      procedure Create_Data_Frame
        (Content : Stream_Element_Array;
         Stop    : in out Boolean);
      --  Create a new data frame from Content

      -----------------------
      -- Create_Data_Frame --
      -----------------------

      procedure Create_Data_Frame
        (Content : Stream_Element_Array;
         Stop    : in out Boolean) is
      begin
         List.Append
           (Frame.Data.Create
              (Stream.Identifier, new Stream_Element_Array'(Content),
               End_Stream => Stop));

         FCW := FCW - Content'Length;

         if FCW <= 0 then
            Stop := True;
         end if;
      end Create_Data_Frame;

      -----------------
      -- From_Stream --
      -----------------

      procedure From_Stream is

         File : Resources.File_Type;

         procedure Send_File is new Server.HTTP_Utils.Send_File_G
           (Create_Data_Frame,
            Chunk_Size => Stream_Element_Count
                            (Positive'Min
                               (FCW, Positive (Ctx.Settings.Max_Frame_Size))));
      begin
         Resources.Streams.Create (File, Self.M_Body);

         Send_File
           (Ctx.HTTP.all, Ctx.Line, File,
            Start  => Stream_Element_Offset (Self.Sent) + 1,
            Length => Resources.Content_Length_Type (Self.Sent));
      end From_Stream;

      --------------------
      -- Handle_Headers --
      --------------------

      procedure Handle_Headers (Headers : AWS.Headers.List) is
         Max_Size : constant Positive :=
                      Connection.Max_Header_List_Size (Ctx.Settings.all);
         L        : AWS.Headers.List;
         Size     : Natural := 0;
         Is_First : Boolean := True;
      begin
         for K in 1 .. Headers.Count loop
            declare
               Element : constant AWS.Headers.Element := Headers.Get (K);
               E_Size  : constant Positive :=
                           32 + Length (Element.Name) + Length (Element.Value);
            begin
               if Debug then
                  Ada.Text_IO.Put_Line
                    ("#hs " & To_String (Element.Name)
                     & ' ' & To_String (Element.Value));
               end if;

               Size := Size + E_Size;

               --  Max header size reached, let's send this as a first frame
               --  and will continue in a continuation frame if necessary.

               if Size > Max_Size then
                  if Is_First then
                     List.Append
                       (Frame.Headers.Create
                          (Ctx.Tab_Enc, Ctx.Settings, Stream.Identifier, L,
                           Flags => (if K = Headers.Count
                                     then Frame.End_Headers_Flag
                                     else 0)));
                     Is_First := False;
                  else
                     List.Append
                       (Frame.Continuation.Create
                          (Ctx.Tab_Enc, Ctx.Settings, Stream.Identifier, L,
                           End_Headers => K = Headers.Count));
                  end if;

                  L.Reset;
                  Size := E_Size;
               end if;

               L.Add (Element.Name, Element.Value);
            end;
         end loop;

         if not L.Is_Empty then
            List.Append
              (Frame.Headers.Create
                 (Ctx.Tab_Enc, Ctx.Settings, Stream.Identifier, L,
                  Flags => Frame.End_Headers_Flag));
         end if;

         if not Self.Has_Body then
            List (List.Last).Set_Flag (Frame.End_Stream_Flag);
         end if;
      end Handle_Headers;

   begin
      if not Self.H_Sent then
         Handle_Headers (Self.Headers);
         Self.H_Sent := True;
      end if;

      if Self.Has_Body and then FCW > 0 then
         From_Stream;
      end if;

      return List;
   end To_Frames;

end AWS.HTTP2.Message;
