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
with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with AWS.Containers.Tables;
with AWS.HTTP2.Connection;
with AWS.HTTP2.Frame.Continuation;
with AWS.HTTP2.Frame.Data;
with AWS.HTTP2.Frame.Headers;
with AWS.HTTP2.Stream;
with AWS.MIME;
with AWS.Resources.Streams.Memory;
with AWS.Response.Set;
with AWS.Server.HTTP_Utils;
with AWS.Translator;

package body AWS.HTTP2.Message is

   function To_Lower
     (Name : String) return String renames Ada.Characters.Handling.To_Lower;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (O : in out Object) is
      use type Utils.Counter_Access;
   begin
      if O.Ref /= null then
         O.Ref.all := O.Ref.all + 1;
      end if;
   end Adjust;

   -----------------
   -- Append_Body --
   -----------------

   procedure Append_Body
     (Self : in out Object;
      Data : String) is
   begin
      if Self.M_Body = null then
         Self.M_Body := new Resources.Streams.Memory.Stream_Type;
      end if;

      Resources.Streams.Memory.Stream_Type (Self.M_Body.all).Append
        (Stream_Element_Array'(Translator.To_Stream_Element_Array (Data)));
   end Append_Body;

   procedure Append_Body
     (Self : in out Object;
      Data : Stream_Element_Array) is
   begin
      if Self.M_Body = null then
         Self.M_Body := new Resources.Streams.Memory.Stream_Type;
      end if;

      Resources.Streams.Memory.Stream_Type (Self.M_Body.all).Append (Data);
   end Append_Body;

   ------------
   -- Create --
   ------------

   function Create
     (Headers   : AWS.Headers.List;
      Data      : Stream_Element_Array;
      Stream_Id : HTTP2.Stream_Id) return Object
   is
      O : Object;
   begin
      O.Kind      := K_Request;
      O.Mode      := Response.Stream;
      O.Stream_Id := Stream_Id;
      O.Headers   := Headers;
      O.Ranges    := Null_Unbounded_String;

      O.Headers.Case_Sensitive (False);

      if Data'Length /= 0 then
         O.M_Body := new Resources.Streams.Memory.Stream_Type;
         Resources.Streams.Memory.Stream_Type (O.M_Body.all).Append (Data);
      end if;

      return O;
   end Create;

   function Create
     (Answer    : in out Response.Data;
      Request   : AWS.Status.Data;
      Stream_Id : HTTP2.Stream_Id) return Object
   is
      O    : Object;
      Size : Stream_Element_Offset := -1;
      --   Size of the resource

      Remove_CE : Boolean := False;
      --  To remove Content-Encoding header in case of file not
      --  found or not changed.

      Remove_CL : Boolean := False;
      --  To remove Content-Length header in case of multiply ranges responce

      Status_Code : Messages.Status_Code := Response.Status_Code (Answer);

      procedure Set_Body;

      --------------
      -- Set_Body --
      --------------

      procedure Set_Body is
      begin
         O.M_Body := Response.Create_Stream
           (Answer, AWS.Status.Is_Supported (Request, Messages.GZip));

         Size := O.M_Body.Size;
      end Set_Body;

      use type Status.Request_Method;

   begin
      O.Kind      := K_Response;
      O.Stream_Id := Stream_Id;
      O.Mode      := (if Status.Method (Request) = Status.HEAD
                      then Response.Header
                      else Response.Mode (Answer));
      O.Ranges    := Null_Unbounded_String;

      O.Headers.Case_Sensitive (False);
      O.Headers.Names_Lowercased (True);

      O.Headers.Add (Messages.Status_Token, Messages.Image (Status_Code));

      case O.Mode is
         when Response.Message | Response.Header =>
            --  Set status code

            if O.Mode = Response.Header then
               Response.Set.Content_Length (Answer);
            else
               Set_Body;

               if Size /= Resources.Undefined_Length then
                  O.Headers.Update
                    (Messages.Content_Length_Token, Utils.Image (Size));
               end if;
            end if;

         when Response.File | Response.File_Once | Response.Stream =>

            declare
               use Ada.Strings;
               use all type Server.HTTP_Utils.Resource_Status;
               use type Ada.Calendar.Time;

               File_Time : Calendar.Time;
               F_Status  : constant Server.HTTP_Utils.Resource_Status :=
                             Server.HTTP_Utils.Get_Resource_Status
                               (Request,
                                Response.Filename (Answer),
                                File_Time);

               With_Body   : Boolean :=
                               Messages.With_Body (Status_Code)
                                 and then Status.Method (Request)
                                 /= Status.HEAD;

               Ranges      : constant String :=
                               AWS.Headers.Get_Values
                                 (Status.Header (Request),
                                  Messages.Range_Token);
               --  The ranges for partial sending if defined
               N_Range     : constant Positive :=
                               1 + Fixed.Count (Ranges, ",");
            begin
               --  Status code header

               case F_Status is
                  when Changed    =>
                     if Ranges /= "" and then With_Body then
                        Status_Code := Messages.S200;
                     end if;

                  when Up_To_Date =>
                     Status_Code := Messages.S304;
                     With_Body := False;
                     Remove_CE := True;

                  when Not_Found  =>
                     Status_Code := Messages.S404;
                     With_Body := False;
                     Remove_CE := True;
               end case;

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

               --  Check if ranges properly defined, if so add necessary
               --  headers.

               if Ranges /= ""
                 and then O.M_Body.Size /= Resources.Undefined_Length
               then
                  declare
                     Boundary : constant String := "aws_range_separator";
                     N_Minus  : constant Natural := Fixed.Count (Ranges, "-");
                     Equal    : constant Natural := Fixed.Index (Ranges, "=");
                     First    : Stream_Element_Offset;
                     Last     : Stream_Element_Offset;
                     R_Length : Stream_Element_Offset;
                  begin
                     if N_Range = N_Minus
                       and then Equal /= 0
                       and then Ranges (Ranges'First .. Equal - 1) = "bytes"
                     then
                        O.Ranges := To_Unbounded_String (Ranges);

                        O.Headers.Add (Messages.Accept_Ranges_Token, "bytes");

                        --  Ignore the Content-Length from Answer part

                        Remove_CL := True;

                        if N_Range = 1 then
                           O.Headers.Add
                             (Messages.Content_Type_Token,
                              Response.Content_Type (Answer));

                           Server.HTTP_Utils.Parse_Content_Range
                             (Ranges (Equal + 1 .. Ranges'Last),
                              Size, First, Last);

                           R_Length := Last - First + 1;

                           --  Add also the Content-Range & Content-Length
                           --  header when there is a single range. For
                           --  multiple range it is not needed as those will
                           --  be set into the body multipart sections.

                           O.Headers.Add
                             (Messages.Content_Range_Token,
                              "bytes "
                              & Utils.Image (First) & "-" & Utils.Image (Last)
                              & "/" & Utils.Image (Size));

                           O.Headers.Update
                             (Messages.Content_Length_Token,
                              Utils.Image (R_Length));

                        else
                           O.Headers.Add
                             (Messages.Content_Type_Token,
                              MIME.Multipart_Byteranges
                              & "; boundary=" & Boundary);
                        end if;
                     end if;
                  end;

               elsif Size /= Resources.Undefined_Length then
                  O.Headers.Update
                    (Messages.Content_Length_Token, Utils.Image (Size));
               end if;
            end;

         when Response.WebSocket =>
            raise Constraint_Error with "websocket is HTTP/1.1 only";

         when Response.Socket_Taken =>
            raise Constraint_Error with "not yet supported";

         when Response.No_Data =>
            raise Constraint_Error with "no_data should never happen";
      end case;

      declare
         List : constant AWS.Headers.List := Response.Header (Answer);
         Item : AWS.Containers.Tables.Element;
         CE   : constant String := To_Lower (Messages.Content_Encoding_Token);
         CL   : constant String := To_Lower (Messages.Content_Length_Token);
      begin
         for J in 1 .. List.Count loop
            Item := List.Get (J);

            declare
               Name : constant String := To_Lower (To_String (Item.Name));
            begin
               --  Remove content encoding and content length on headers union
               --  if needed and always remove the status-code which must be
               --  updated.

               if (not Remove_CE or else Name /= CE)
                 and then (not Remove_CL or else Name /= CL)
                 and then Name /= Messages.Status_Token
               then
                  if Name in CL | CE then
                     --  Do not duplicate Content-Length and Content-Encoding
                     --  headers.

                     O.Headers.Update (Item.Name, Item.Value);

                  else
                     O.Headers.Add (Item.Name, Item.Value);
                  end if;
               end if;
            end;
         end loop;
      end;

      --  Set status-code

      Response.Set.Status_Code (Answer, Status_Code);
      O.Headers.Update (Messages.Status_Token, Messages.Image (Status_Code));

      return O;
   end Create;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (O : in out Object) is
      use type Utils.Counter_Access;
      C : Utils.Counter_Access := O.Ref;
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Resources.Streams.Stream_Type'Class, Resources.Streams.Stream_Access);
   begin
      if C /= null then
         O.Ref := null;

         C.all := C.all - 1;

         if C.all = 0 then
            if O.M_Body /= null then
               O.M_Body.Close;
               Unchecked_Free (O.M_Body);
            end if;

            O.Headers.Reset;
            Utils.Unchecked_Free (C);
         end if;
      end if;
   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (O : in out Object) is
   begin
      O.Ref := new Natural'(1);
   end Initialize;

   ---------------
   -- To_Frames --
   ---------------

   function To_Frames
     (Self   : in out Object;
      Ctx    : Server.Context.Object;
      Stream : HTTP2.Stream.Object)
      return AWS.HTTP2.Frame.List.Object
   is
      use type HTTP2.Frame.Kind_Type;

      FCW  : Natural :=
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
        (Content   : Stream_Element_Array;
         Next_Size : in out Stream_Element_Count);
      --  Create a new data frame from Content

      -----------------------
      -- Create_Data_Frame --
      -----------------------

      procedure Create_Data_Frame
        (Content   : Stream_Element_Array;
         Next_Size : in out Stream_Element_Count) is
      begin
         if Next_Size = 0 then
            Self.B_Sent := True;
         end if;

         List.Append
           (Frame.Data.Create
              (Stream.Identifier, new Stream_Element_Array'(Content),
               End_Stream => Next_Size = 0));

         FCW := FCW - Content'Length;

         if FCW < Natural (Next_Size) then
            Next_Size := Stream_Element_Count (FCW);
         end if;
      end Create_Data_Frame;

      -----------------
      -- From_Stream --
      -----------------

      procedure From_Stream is

         File : Resources.File_Type;

         procedure Send_File is new Server.HTTP_Utils.Send_File_G
           (Create_Data_Frame);

         procedure Send_Ranges is new Server.HTTP_Utils.Send_File_Ranges_G
           (Create_Data_Frame, Send_File, True);

         Length : Resources.Content_Length_Type;
         Dummy  : Response.Data;

         function Chunk_Size return Stream_Element_Count is
           (Stream_Element_Count
              (Positive'Min (FCW, Positive (Ctx.Settings.Max_Frame_Size))));

      begin
         Resources.Streams.Create (File, Self.M_Body);

         if Self.Ranges /= Null_Unbounded_String then
            Length := Resources.Size (File);

            Send_Ranges
              (Ctx.HTTP, Ctx.Line, File,
               To_String (Self.Ranges), Chunk_Size, Length, Dummy);

         else
            Send_File
              (Ctx.HTTP, Ctx.Line, File,
               Start      => Stream_Element_Offset (Self.Sent) + 1,
               Chunk_Size => Chunk_Size,
               Length     => Resources.Content_Length_Type (Self.Sent));
         end if;
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
         L.Case_Sensitive (False);

         for K in 1 .. Headers.Count loop
            declare
               Element : constant AWS.Headers.Element := Headers.Get (K);
               E_Size  : constant Positive :=
                           32 + Length (Element.Name) + Length (Element.Value);
            begin
               if Debug then
                  Text_IO.Put_Line
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
         if not Self.Headers.Exist (Messages.Content_Length_Token)
           and then Self.Ranges = Null_Unbounded_String
           and then Self.M_Body /= null
         then
            declare
               Size : constant Stream_Element_Offset := Self.M_Body.Size;
            begin
               if Size /= Resources.Undefined_Length then
                  Self.Headers.Add
                    (To_Lower (Messages.Content_Length_Token),
                     Utils.Image (Size));
               end if;
            end;
         end if;

         Handle_Headers (Self.Headers);
         Self.H_Sent := True;
      end if;

      if Self.Has_Body and then FCW > 0 then
         From_Stream;

         --  If file is empty the last frame is the header one

         if List (List.Last).Kind = HTTP2.Frame.K_Headers then
            List (List.Last).Set_Flag (Frame.End_Stream_Flag);
         end if;
      end if;

      return List;
   end To_Frames;

end AWS.HTTP2.Message;
