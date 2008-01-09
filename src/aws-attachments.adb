------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2004-2008                          --
--                                 AdaCore                                  --
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

with Ada.Calendar;
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.IO_Exceptions;

with GNAT.Calendar.Time_IO;

with AWS.Headers.Set;
with AWS.Headers.Values;
with AWS.Messages;
with AWS.MIME;
with AWS.Net.Buffered;
with AWS.Translator;
with AWS.Utils;

package body AWS.Attachments is

   function Value (V : in String) return Unbounded_String;
   --  Returns V as an Unbounded_String if V is not the empty string
   --  otherwise it returns Null_Unbounded_String.

   UID : Utils.Counter (0);
   --  Unique Id used for generating the MIME boundaries

   ---------
   -- Add --
   ---------

   procedure Add
     (Attachments : in out List;
      Filename    : in     String;
      Content_Id  : in     String;
      Encode      : in     Encoding := None)
   is
      use type Attachment_Table.Vector;

      Base_Filename : constant String := Directories.Simple_Name (Filename);
      File_Size     : Natural;
      Tmp           : Element;
   begin
      --  Get the file size first, this will raise an exception
      --  if the file is missing.

      File_Size := Natural (Utils.File_Size (Filename));

      Tmp.Filename := Value (Filename);
      Tmp.Encode := Encode;

      AWS.Headers.Set.Add
        (Headers => Tmp.Headers,
         Name    => AWS.Messages.Content_Type_Token,
         Value   => MIME.Content_Type (Filename)
           & "; name=""" & Base_Filename & '"');

      if Encode = Base64 then
         AWS.Headers.Set.Add
           (Headers => Tmp.Headers,
            Name    => AWS.Messages.Content_Transfer_Encoding_Token,
            Value   => "base64");

      else
         if MIME.Is_Text (MIME.Content_Type (Filename)) then
            AWS.Headers.Set.Add
              (Headers => Tmp.Headers,
               Name    => AWS.Messages.Content_Transfer_Encoding_Token,
               Value   => "8bit");
         else
            AWS.Headers.Set.Add
              (Headers => Tmp.Headers,
               Name    => AWS.Messages.Content_Transfer_Encoding_Token,
               Value   => "binary");
         end if;
      end if;

      AWS.Headers.Set.Add
        (Headers => Tmp.Headers,
         Name    => AWS.Messages.Content_Disposition_Token,
         Value   => "attachment; filename=""" & Base_Filename & '"');

      AWS.Headers.Set.Add
        (Headers => Tmp.Headers,
         Name    => AWS.Messages.Content_Id_Token,
         Value   => '<' & Content_Id & '>');

      Tmp.Total_Length := AWS.Headers.Length (Tmp.Headers) + File_Size;

      Attachment_Table.Append (Attachments.Vector, Tmp);
   end Add;

   procedure Add
     (Attachments : in out List;
      Filename    : in     String;
      Headers     : in     AWS.Headers.List;
      Encode      : in     Encoding := None)
   is
      File_Size : Natural;
   begin
      --  Get the file size first, this will raise an exception
      --  if the file is missing.

      File_Size := Natural (Utils.File_Size (Filename));

      Attachment_Table.Append
        (Attachments.Vector,
         (Kind         => File,
          Filename     => Value (Filename),
          Headers      => Headers,
          Total_Length => AWS.Headers.Length (Headers) + File_Size,
          Encode       => Encode));
   end Add;

   procedure Add
     (Attachments  : in out List;
      Content      : in     String;
      Content_Type : in     String)
   is
      Tmp : Element (AWS.Attachments.Content);
   begin
      AWS.Headers.Set.Add
        (Headers => Tmp.Headers,
         Name    => AWS.Messages.Content_Type_Token,
         Value   => Content_Type);

      Tmp.Encoding := None;
      Tmp.Content := To_Unbounded_String (Content);
      Tmp.Total_Length := AWS.Headers.Length (Tmp.Headers) + Content'Length;

      Attachments.Vector.Append (Tmp);
   end Add;

   procedure Add
     (Parts        : in out Alternatives;
      Content      : in     String;
      Content_Type : in     String) is
   begin
      Parts.Parts.Append
        (Alternative_Part'
           (Content     => To_Unbounded_String (Content),
            Content_Type => To_Unbounded_String (Content_Type)));
   end Add;

   procedure Add
     (Attachments : in out List;
      Parts       : in     Alternatives) is
   begin
      Attachments.Vector.Append (Element (Parts));
   end Add;

   ----------------
   -- Add_Base64 --
   ----------------

   procedure Add_Base64
     (Attachments : in out List;
      Name        : in     String;
      Content     : in     String)
   is
      Base_Filename : constant String := Directories.Simple_Name (Name);
      Tmp           : Element (AWS.Attachments.Content);
   begin
      AWS.Headers.Set.Add
        (Headers => Tmp.Headers,
         Name    => AWS.Messages.Content_Type_Token,
         Value   => MIME.Content_Type (Name) & "; name="""
           &  Base_Filename & '"');

      AWS.Headers.Set.Add
        (Headers => Tmp.Headers,
         Name    => AWS.Messages.Content_Transfer_Encoding_Token,
         Value   => "base64");

      AWS.Headers.Set.Add
        (Headers => Tmp.Headers,
         Name    => AWS.Messages.Content_Disposition_Token,
         Value   => "attachment; filename=""" & Base_Filename & '"');

      Tmp.Encoding := Base64;
      Tmp.Content := To_Unbounded_String (Content);
      Tmp.Total_Length := AWS.Headers.Length (Tmp.Headers) + Content'Length;

      Attachments.Vector.Append (Tmp);
   end Add_Base64;

   ----------------
   -- Content_Id --
   ----------------

   function Content_Id (Attachment : in Element) return String is
   begin
      return AWS.Headers.Get (Attachment.Headers, Messages.Content_Id_Token);
   end Content_Id;

   ------------------
   -- Content_Type --
   ------------------

   function Content_Type (Attachment : in Element) return String is
   begin
      return AWS.Headers.Values.Get_Unnamed_Value
        (AWS.Headers.Get (Attachment.Headers, Messages.Content_Type_Token));
   end Content_Type;

   -----------
   -- Count --
   -----------

   function Count (Attachments : in List) return Natural is
   begin
      return Natural (Attachment_Table.Length (Attachments.Vector));
   end Count;

   --------------
   -- Filename --
   --------------

   function Filename (Attachment : in Element) return String is
      Result : Unbounded_String;
   begin
      if AWS.Headers.Exist
        (Attachment.Headers, Messages.Content_Disposition_Token)
      then
         Result := Value
           (AWS.Headers.Values.Search
              (AWS.Headers.Get
                 (Attachment.Headers, Messages.Content_Disposition_Token),
               "filename"));
      end if;

      if Result = Null_Unbounded_String
        and then
          AWS.Headers.Exist
            (Attachment.Headers, Messages.Content_Type_Token)
      then
         Result := Value
           (AWS.Headers.Values.Search
              (AWS.Headers.Get
                 (Attachment.Headers, Messages.Content_Type_Token),
               "name"));
      end if;

      return To_String (Result);
   end Filename;

   --------------------------
   -- For_Every_Attachment --
   --------------------------

   procedure For_Every_Attachment (Attachments : in List) is
      Quit : Boolean := False;
   begin
      for J in 1 .. Count (Attachments) loop
         Action (Get (Attachments, J), J, Quit);
         exit when Quit;
      end loop;
   end For_Every_Attachment;

   ---------
   -- Get --
   ---------

   function Get
     (Attachments : in List;
      Index       : in Positive) return Element is
   begin
      return Attachment_Table.Element
        (Container => Attachments.Vector, Index => Index);
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Attachments : in List;
      Content_Id  : in String) return Element
   is

      function Get_CID (Content_Id : in String) return String;
      --  Returns Content_Id stripped from possibly surrounding
      --  '<' '>' and prefixing "cid:".

      -------------
      -- Get_CID --
      -------------

      function Get_CID (Content_Id : in String) return String is
      begin
         if Content_Id (Content_Id'First) = '<'
           and then Content_Id (Content_Id'Last) = '>'
         then
            return Get_CID
              (Content_Id (Content_Id'First + 1 .. Content_Id'Last - 1));

         elsif Content_Id'Length > 4
           and then
             Content_Id (Content_Id'First .. Content_Id'First + 3) = "cid:"
         then
            return Content_Id (Content_Id'First + 4 .. Content_Id'Last);
         else
            return Content_Id;
         end if;
      end Get_CID;

      CID        : constant String := Get_CID (Content_Id);
      Attachment : Element;

   begin
      for J in 1 .. Count (Attachments) loop
         Attachment := Get (Attachments, J);

         if AWS.Attachments.Content_Id (Attachment) = CID
           or else AWS.Attachments.Content_Id (Attachment) = '<' & CID & '>'
         then
            return Attachment;
         end if;
      end loop;

      --  No Content ID matched the request

      raise Constraint_Error;
   end Get;

   -------------
   -- Headers --
   -------------

   function Headers (Attachment : in Element) return AWS.Headers.List is
   begin
      return Attachment.Headers;
   end Headers;

   ----------
   -- Kind --
   ----------

   function Kind (Attachment : in Element) return Attachment_Kind is
   begin
      return Attachment.Kind;
   end Kind;

   ------------
   -- Length --
   ------------

   function Length
     (Attachments : in List;
      Boundary    : in String) return Natural
   is
      L : Natural;
   begin
      --  The length of all start boundaries, and the 2 characters longer
      --  end boundary.

      L := (Boundary'Length + 6) * (Count (Attachments) + 1) + 2;

      --  Add all content lengths

      for J in 1 .. Count (Attachments) loop
         L := L + Element (Element'(Get (Attachments, J))).Total_Length;
      end loop;

      return L;
   end Length;

   --------------------
   -- Local_Filename --
   --------------------

   function Local_Filename (Attachment : in Element) return String is
   begin
      return To_String (Attachment.Filename);
   end Local_Filename;

   -----------
   -- Reset --
   -----------

   procedure Reset
     (Attachments  : in out List;
      Delete_Files : in     Boolean) is
   begin
      if Delete_Files then
         for J in 1 .. Count (Attachments) loop
            begin
               Directories.Delete_File
                 (Name => Local_Filename (Element'(Get (Attachments, J))));
            exception
               when IO_Exceptions.Name_Error =>
                  null;
            end;
         end loop;
      end if;
      Attachment_Table.Clear (Attachments.Vector);
   end Reset;

   ---------------
   -- Root_MIME --
   ---------------

   function Root_MIME (Attachments : in List) return Root_MIME_Kind is
   begin
      if Count (Attachments) = 1
        and then Kind (Element'(Get (Attachments, 1))) = Alternative
      then
         return Multipart_Alternative;
      else
         return Multipart_Mixed;
      end if;
   end Root_MIME;

   ----------
   -- Send --
   ----------

   procedure Send
     (Socket      : in AWS.Net.Socket_Type'Class;
      Attachments : in List;
      Boundary    : in String)
   is

      procedure Send_Attachment (Attachment : in Element);
      --  Sends one Attachment, including the start boundary

      procedure Send_File (Attachment : in Element);
      --  Send a file

      procedure Send_Content (Attachment : in Element);
      --  Set an in-memory content

      procedure Send_Alternative (Attachment : in Element);
      --  Send an alternative part

      Pref_Suf : constant String := "--";
      --  The MIME boundary prefix and suffix

      Simple_Alternative : constant Boolean :=
                             Root_MIME (Attachments) = Multipart_Alternative;

      ----------------------
      -- Send_Alternative --
      ----------------------

      procedure Send_Alternative (Attachment : in Element) is

         procedure Send_Alternative (Position : in Alternative_Table.Cursor);
         --  Output the pointed part

         A_Boundary : Unbounded_String;

         ----------------------
         -- Send_Alternative --
         ----------------------

         procedure Send_Alternative (Position : in Alternative_Table.Cursor) is
            Part : constant Alternative_Part :=
                     Alternative_Table.Element (Position);
         begin
            Net.Buffered.Put_Line (Socket, "--" & To_String (A_Boundary));
            Net.Buffered.Put_Line
              (Socket, Messages.Content_Type (To_String (Part.Content_Type)));
            Net.Buffered.New_Line (Socket);

            Net.Buffered.Put_Line (Socket, To_String (Part.Content));
         end Send_Alternative;

      begin
         if not Simple_Alternative then
            --  This is not the first element, we issue an embedded MIME
            --  content.
            Net.Buffered.Put_Line (Socket, "--" & Boundary);
            Send_MIME_Header
              (Socket, Attachments,
               Alternative => True,
               Boundary    => A_Boundary);
         else
            A_Boundary := To_Unbounded_String (Boundary);
         end if;

         Attachment.Parts.Iterate (Send_Alternative'Access);

         if not Simple_Alternative then
            --  Ends the alternative part
            Net.Buffered.New_Line (Socket);
            Net.Buffered.Put_Line
              (Socket, "--" & To_String (A_Boundary) & "--");
         end if;
      end Send_Alternative;

      ---------------------
      -- Send_Attachment --
      ---------------------

      procedure Send_Attachment (Attachment : in Element) is
      begin
         case Attachment.Kind is
            when File        => Send_File (Attachment);
            when Content     => Send_Content (Attachment);
            when Alternative => Send_Alternative (Attachment);
         end case;
      end Send_Attachment;

      ------------------
      -- Send_Content --
      ------------------

      procedure Send_Content (Attachment : in Element) is

         procedure Send_Content;
         --  Send standard content

         procedure Send_Base64_Content;
         --  Send a base64 content

         -------------------------
         -- Send_Base64_Content --
         -------------------------

         procedure Send_Base64_Content is
            Chunk_Size  : constant := 60;
            Content_Len : constant Positive := Length (Attachment.Content);
            K           : Positive := 1;
         begin
            while K <= Content_Len loop
               if K + Chunk_Size - 1 > Content_Len then
                  Net.Buffered.Put_Line
                    (Socket,
                     Slice (Attachment.Content, K, Content_Len));
                  K := Content_Len + 1;
               else
                  Net.Buffered.Put_Line
                    (Socket,
                     Slice (Attachment.Content, K, K + Chunk_Size - 1));
                  K := K + Chunk_Size;
               end if;
            end loop;
         end Send_Base64_Content;

         ------------------
         -- Send_Content --
         ------------------

         procedure Send_Content is
         begin
            Net.Buffered.Put (Socket, To_String (Attachment.Content));
         end Send_Content;

      begin
         --  Send multipart message start boundary

         Net.Buffered.New_Line (Socket);
         Net.Buffered.Put_Line (Socket, Pref_Suf & Boundary);

         --  Send header

         AWS.Headers.Send_Header (Socket, Attachment.Headers);
         Net.Buffered.New_Line (Socket);

         case Attachment.Encoding is
            when None   => Send_Content;
            when Base64 => Send_Base64_Content;
         end case;

         Net.Buffered.New_Line (Socket);
      end Send_Content;

      ---------------
      -- Send_File --
      ---------------

      procedure Send_File (Attachment : in Element) is

         procedure Send_File;
         --  Send file as-is

         procedure Send_File_Base64;
         --  Send file encoded in Base64

         File : Streams.Stream_IO.File_Type;

         ---------------
         -- Send_File --
         ---------------

         procedure Send_File is
            Buffer : Streams.Stream_Element_Array (1 .. 4_096);
            Last   : Streams.Stream_Element_Offset;
         begin
            --  Send file content

            while not Streams.Stream_IO.End_Of_File (File) loop
               Streams.Stream_IO.Read (File, Buffer, Last);
               Net.Buffered.Write (Socket, Buffer (1 .. Last));
            end loop;
         exception
            when Net.Socket_Error =>
               --  Properly close the file if needed
               if Streams.Stream_IO.Is_Open (File) then
                  Streams.Stream_IO.Close (File);
               end if;
               raise;
         end Send_File;

         ----------------------
         -- Send_File_Base64 --
         ----------------------

         procedure Send_File_Base64 is
            Buffer_Size   : constant := 60;
            --  Note that this size must be a multiple of 3, this is important
            --  to have proper chunk MIME encoding.

            Buffer : Streams.Stream_Element_Array (1 .. Buffer_Size);
            Last   : Streams.Stream_Element_Offset;
         begin
            while not Streams.Stream_IO.End_Of_File (File) loop
               Streams.Stream_IO.Read (File, Buffer, Last);

               Net.Buffered.Put_Line
                 (Socket, AWS.Translator.Base64_Encode (Buffer (1 .. Last)));
            end loop;

            Net.Buffered.New_Line (Socket);
         end Send_File_Base64;

      begin
         --  Send multipart message start boundary

         Net.Buffered.New_Line (Socket);
         Net.Buffered.Put_Line (Socket, Pref_Suf & Boundary);

         --  Send header

         AWS.Headers.Send_Header (Socket, Attachment.Headers);
         Net.Buffered.New_Line (Socket);

         Streams.Stream_IO.Open
           (File, Streams.Stream_IO.In_File, To_String (Attachment.Filename));

         case Attachment.Encode is
            when None   => Send_File;
            when Base64 => Send_File_Base64;
         end case;

         Streams.Stream_IO.Close (File);
      end Send_File;

   begin
      --  Send the attachments

      for J in
        1 .. Integer (Attachment_Table.Length (Attachments.Vector))
      loop
         Send_Attachment
           (Attachment_Table.Element
              (Container => Attachments.Vector,
               Index     => J));
      end loop;

      --  Send multipart message end boundary

      Net.Buffered.New_Line (Socket);

      Net.Buffered.Put_Line (Socket, Pref_Suf & Boundary & Pref_Suf);
   end Send;

   ----------------------
   -- Send_MIME_Header --
   ----------------------

   procedure Send_MIME_Header
     (Socket      : in     Net.Socket_Type'Class;
      Attachments : in     List;
      Boundary    :    out Unbounded_String;
      Alternative : in     Boolean := False)
   is
      use type GNAT.Calendar.Time_IO.Picture_String;
      L_Boundary : constant String :=
                     GNAT.Calendar.Time_IO.Image
                       (Calendar.Clock, "----=_NextPart_%s."
                        & GNAT.Calendar.Time_IO.Picture_String
                          (Utils.Image (UID.Value)));
   begin
      UID.Increment;

      Boundary := To_Unbounded_String (L_Boundary);

      if Alternative
        or else Root_MIME (Attachments) = Multipart_Alternative
      then
         Net.Buffered.Put_Line
           (Socket,
            Messages.Content_Type ("multipart/alternative", L_Boundary));
      else
         Net.Buffered.Put_Line
           (Socket,
            Messages.Content_Type ("multipart/mixed", L_Boundary));
      end if;

      Net.Buffered.New_Line (Socket);
   end Send_MIME_Header;

   -----------
   -- Value --
   -----------

   function Value (V : in String) return Unbounded_String is
   begin
      if V = "" then
         return Null_Unbounded_String;
      else
         return To_Unbounded_String (V);
      end if;
   end Value;

end AWS.Attachments;
