------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2009, AdaCore                     --
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

with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.IO_Exceptions;

with AWS.Headers.Set;
with AWS.Headers.Values;
with AWS.Messages;
with AWS.Net.Buffered;
with AWS.Translator;
with AWS.Utils;

package body AWS.Attachments is

   use Ada.Streams;

   function "+"
     (V : in String)
      return Unbounded_String renames To_Unbounded_String;

   UID : Utils.Counter (0);
   --  Unique Id used for generating the MIME boundaries

   ---------
   -- Add --
   ---------

   procedure Add
     (Attachments : in out List;
      Filename    : in     String;
      Content_Id  : in     String;
      Headers     : in     AWS.Headers.List := AWS.Headers.Empty_List;
      Name        : in     String := "";
      Encode      : in     Encoding := None)
   is
      use type Attachment_Table.Vector;

      Data : constant Content := File
        (Filename, Encode, Content_Id, MIME.Content_Type (Filename));
   begin
      if Name = "" then
         Add (Attachments, Filename, Data, Headers);
      else
         Add (Attachments, Name, Data, Headers);
      end if;
   end Add;

   procedure Add
     (Attachments : in out List;
      Filename    : in     String;
      Headers     : in     AWS.Headers.List;
      Name        : in     String := "";
      Encode      : in     Encoding := None)
   is
      Data : constant Content := File
        (Filename, Encode, "", MIME.Content_Type (Filename));
   begin
      if Name = "" then
         Add (Attachments, Filename, Data, Headers);
      else
         Add (Attachments, Name, Data, Headers);
      end if;
   end Add;

   procedure Add
     (Attachments : in out List;
      Name        : in     String;
      Data        : in     Content;
      Headers     : in     AWS.Headers.List := AWS.Headers.Empty_List)
   is
      A : Element :=
            (AWS.Attachments.Data, Headers,
             Data.Length + AWS.Headers.Length (Headers), Data);
   begin
      if Data.Filename = Null_Unbounded_String then
         if Data.Content_Type /= Null_Unbounded_String then
            AWS.Headers.Set.Add
              (Headers => A.Headers,
               Name    => AWS.Messages.Content_Type_Token,
               Value   => To_String (Data.Content_Type));
         end if;

      else
         AWS.Headers.Set.Add
           (Headers => A.Headers,
            Name    => AWS.Messages.Content_Disposition_Token,
            Value   => "attachment; filename=""" & Name & '"');

         if Data.Content_Type = Null_Unbounded_String then
            AWS.Headers.Set.Add
              (Headers => A.Headers,
               Name    => AWS.Messages.Content_Type_Token,
               Value   => MIME.Content_Type (Name) & "; name="""
               &  Name & '"');
         else
            AWS.Headers.Set.Add
              (Headers => A.Headers,
               Name    => AWS.Messages.Content_Type_Token,
               Value   => To_String (Data.Content_Type) & "; name="""
               &  Name & '"');
         end if;

         if Data.Encode = None then
            if MIME.Is_Text (MIME.Content_Type (Name)) then
               AWS.Headers.Set.Add
                 (Headers => A.Headers,
                  Name    => AWS.Messages.Content_Transfer_Encoding_Token,
                  Value   => "8bit");
            else
               AWS.Headers.Set.Add
                 (Headers => A.Headers,
                  Name    => AWS.Messages.Content_Transfer_Encoding_Token,
                  Value   => "binary");
            end if;
         end if;
      end if;

      if Data.Encode = Base64 then
         AWS.Headers.Set.Add
           (Headers => A.Headers,
            Name    => AWS.Messages.Content_Transfer_Encoding_Token,
            Value   => "base64");
      end if;

      if Data.Content_Id /= Null_Unbounded_String then
         AWS.Headers.Set.Add
           (Headers => A.Headers,
            Name    => AWS.Messages.Content_Id_Token,
            Value   => '<' & To_String (Data.Content_Id) & '>');
      end if;

      Attachments.Vector.Append (A);
   end Add;

   procedure Add
     (Parts : in out Alternatives;
      Data  : in     Content) is
   begin
      Parts.Parts.Append (Data);
   end Add;

   procedure Add
     (Attachments : in out List;
      Parts       : in     Alternatives) is
   begin
      Attachments.Vector.Append (Element (Parts));
   end Add;

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

   ----------
   -- File --
   ----------

   function File
     (Filename     : in String;
      Encode       : in Encoding := None;
      Content_Id   : in String := "";
      Content_Type : in String := MIME.Text_Plain) return Content is
   begin
      return Content'(File, Natural (Utils.File_Size (Filename)),
                      Content_Id   => +Content_Id,
                      Content_Type => +Content_Type,
                      Filename     => +Filename,
                      Encode       => Encode);
   end File;

   --------------
   -- Filename --
   --------------

   function Filename (Attachment : in Element) return String is
      Result : Unbounded_String;
   begin
      if AWS.Headers.Exist
        (Attachment.Headers, Messages.Content_Disposition_Token)
      then
         Result :=
           +(AWS.Headers.Values.Search
             (AWS.Headers.Get
              (Attachment.Headers, Messages.Content_Disposition_Token),
               "filename"));
      end if;

      if Result = Null_Unbounded_String
        and then
          AWS.Headers.Exist
            (Attachment.Headers, Messages.Content_Type_Token)
      then
         Result :=
           +(AWS.Headers.Values.Search
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

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Attachments : in List;
      Process     : not null access procedure (Attachment : in Element))
   is
      --  Use callbacks to avoid Elements copy on iteration

      procedure Action (Position : in Attachment_Table.Cursor);

      ------------
      -- Action --
      ------------

      procedure Action (Position : in Attachment_Table.Cursor) is
      begin
         Attachment_Table.Query_Element (Position, Process);
      end Action;

   begin
      Attachments.Vector.Iterate (Action'Access);
   end Iterate;

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
      return To_String (Attachment.Data.Filename);
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
               when IO_Exceptions.Name_Error | IO_Exceptions.Use_Error =>
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

      procedure Send_Content (Attachment : in Element);
      --  Set an in-memory content

      procedure Send_Content (Data : in Content);

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
            Part : constant Content :=
                     Alternative_Table.Element (Position);
         begin
            Net.Buffered.Put_Line (Socket, Pref_Suf & To_String (A_Boundary));
            Net.Buffered.Put_Line
              (Socket, Messages.Content_Type (To_String (Part.Content_Type)));
            Net.Buffered.New_Line (Socket);

            Send_Content (Part);
         end Send_Alternative;

      begin
         if not Simple_Alternative then
            --  This is not the first element, we issue an embedded MIME
            --  content.
            Net.Buffered.Put_Line (Socket, Pref_Suf & Boundary);
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
              (Socket, Pref_Suf & To_String (A_Boundary) & Pref_Suf);
         end if;
      end Send_Alternative;

      ---------------------
      -- Send_Attachment --
      ---------------------

      procedure Send_Attachment (Attachment : in Element) is
      begin
         case Attachment.Kind is
            when Data        => Send_Content (Attachment);
            when Alternative => Send_Alternative (Attachment);
         end case;
      end Send_Attachment;

      ------------------
      -- Send_Content --
      ------------------

      procedure Send_Content (Attachment : in Element) is

      begin
         --  Send multipart message start boundary

         Net.Buffered.Put_Line (Socket, Pref_Suf & Boundary);

         --  Send header

         AWS.Headers.Send_Header (Socket, Attachment.Headers);
         Net.Buffered.New_Line (Socket);

         Send_Content (Attachment.Data);
      end Send_Content;

      procedure Send_Content (Data : in Content) is

         procedure Send_File;

         procedure Send_Content;

         ------------------
         -- Send_Content --
         ------------------

         procedure Send_Content is

            procedure Send;
            --  Send standard content

            procedure Send_Base64;
            --  Send a base64 content

            ----------
            -- Send --
            ----------

            procedure Send is
            begin
               Net.Buffered.Put_Line (Socket, To_String (Data.Content));
            end Send;

            -----------------
            -- Send_Base64 --
            -----------------

            procedure Send_Base64 is
               Chunk_Size  : constant := 60;
               Content_Len : constant Positive := Length (Data.Content);
               K           : Positive := 1;
            begin
               while K <= Content_Len loop
                  if K + Chunk_Size - 1 > Content_Len then
                     Net.Buffered.Put_Line
                       (Socket,
                        Slice (Data.Content, K, Content_Len));
                     K := Content_Len + 1;
                  else
                     Net.Buffered.Put_Line
                       (Socket,
                        Slice (Data.Content, K, K + Chunk_Size - 1));
                     K := K + Chunk_Size;
                  end if;
               end loop;
            end Send_Base64;

         begin
            case Data.Encode is
               when None   => Send;
               when Base64 => Send_Base64;
            end case;
         end Send_Content;

         ---------------
         -- Send_File --
         ---------------

         procedure Send_File is

            procedure Send;
            --  Send file as-is

            procedure Send_Base64;
            --  Send file encoded in Base64

            File : Streams.Stream_IO.File_Type;

            ----------
            -- Send --
            ----------

            procedure Send is
               Buffer : Streams.Stream_Element_Array (1 .. 4_096);
               Last   : Streams.Stream_Element_Offset;
            begin
               --  Send file content

               while not Streams.Stream_IO.End_Of_File (File) loop
                  Streams.Stream_IO.Read (File, Buffer, Last);
                  Net.Buffered.Write (Socket, Buffer (1 .. Last));
               end loop;

               Net.Buffered.New_Line (Socket);
            exception
               when Net.Socket_Error =>
                  --  Properly close the file if needed
                  if Streams.Stream_IO.Is_Open (File) then
                     Streams.Stream_IO.Close (File);
                  end if;
                  raise;
            end Send;

            -----------------
            -- Send_Base64 --
            -----------------

            procedure Send_Base64 is
               Buffer_Size   : constant := 60;
               --  Note that this size must be a multiple of 3, this is
               --  important to have proper chunk MIME encoding.

               Buffer : Streams.Stream_Element_Array (1 .. Buffer_Size);
               Last   : Streams.Stream_Element_Offset;
            begin
               while not Streams.Stream_IO.End_Of_File (File) loop
                  Streams.Stream_IO.Read (File, Buffer, Last);

                  Net.Buffered.Put_Line
                    (Socket,
                     AWS.Translator.Base64_Encode (Buffer (1 .. Last)));
               end loop;
            end Send_Base64;

         begin
            Stream_IO.Open
              (File, Streams.Stream_IO.In_File, To_String (Data.Filename));

            case Data.Encode is
               when None   => Send;
               when Base64 => Send_Base64;
            end case;

            Stream_IO.Close (File);
         end Send_File;

      begin
         case Data.Kind is
            when File                 => Send_File;
            when AWS.Attachments.Data => Send_Content;
         end case;
      end Send_Content;

   begin
      --  Send the attachments

      for J in 1 .. Integer (Attachments.Vector.Length) loop
         Send_Attachment
           (Attachment_Table.Element
              (Container => Attachments.Vector,
               Index     => J));
      end loop;

      --  Send multipart message end boundary

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
      L_Boundary : constant String :=
        "----=_NextPart_" & Utils.Random_String (10) & "."
        & Utils.Image (UID.Value);
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

   ----------
   -- Data --
   ----------

   function Value
     (Data         : in String;
      Name         : in String := "";
      Encode       : in Encoding := None;
      Content_Id   : in String := "";
      Content_Type : in String := MIME.Text_Plain) return Content
   is
      CD : Unbounded_String;
   begin
      if Encode = Base64 then
         CD := +Translator.Base64_Encode (Data);
      else
         CD := +Data;
      end if;

      return Content'(Attachments.Data, Data'Length,
                      Content_Id   => +Content_Id,
                      Content_Type => +Content_Type,
                      Filename     => +Name,
                      Encode       => Encode,
                      Content      => CD);
   end Value;

end AWS.Attachments;
