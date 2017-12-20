------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2017, AdaCore                     --
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

pragma Ada_2012;

with Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Streams.Stream_IO;

with AWS.Headers.Values;
with AWS.Messages;
with AWS.Net.Buffered;
with AWS.Translator;
with AWS.Utils;

package body AWS.Attachments is

   use Ada.Streams;

   function "+"
     (V : String) return Unbounded_String renames To_Unbounded_String;

   UID : Utils.Counter (0);
   --  Unique Id used for generating the MIME boundaries

   ---------
   -- Add --
   ---------

   procedure Add
     (Attachments : in out List;
      Filename    : String;
      Content_Id  : String;
      Headers     : AWS.Headers.List := AWS.Headers.Empty_List;
      Name        : String := "";
      Encode      : Encoding := None)
   is
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
      Filename    : String;
      Headers     : AWS.Headers.List;
      Name        : String := "";
      Encode      : Encoding := None)
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
      Name        : String;
      Data        : Content;
      Headers     : AWS.Headers.List := AWS.Headers.Empty_List)
   is
      Has_Content_Type : constant Boolean :=
                           Headers.Exist (AWS.Messages.Content_Type_Token);
      Has_Content_TE   : constant Boolean :=
                           Headers.Exist
                             (AWS.Messages.Content_Transfer_Encoding_Token);
      A                : Element :=
                           (AWS.Attachments.Data, Headers,
                            Data.Length + AWS.Headers.Length (Headers), Data);
   begin
      if Data.Filename = Null_Unbounded_String then
         if Data.Content_Type /= Null_Unbounded_String then
            A.Headers.Update
              (Name  => AWS.Messages.Content_Type_Token,
               Value => To_String (Data.Content_Type));
         end if;

      else
         A.Headers.Add
           (Name  => AWS.Messages.Content_Disposition_Token,
            Value => "attachment; filename=""" & Name & '"');

         if Data.Content_Type = Null_Unbounded_String then
            if not Has_Content_Type then
               --  Content_Type is not in A.Headers nor defined with Data,
               --  create one based on the file name.

               A.Headers.Add
                 (Name  => AWS.Messages.Content_Type_Token,
                  Value => MIME.Content_Type (Name) & "; name="""
                    &  Name & '"');
            end if;

         else
            A.Headers.Update
              (Name  => AWS.Messages.Content_Type_Token,
               Value => To_String (Data.Content_Type) & "; name="""
                 &  Name & '"');
         end if;

         if Data.Encode = None and then not Has_Content_TE then
            if MIME.Is_Text (MIME.Content_Type (Name)) then
               A.Headers.Add
                 (Name  => AWS.Messages.Content_Transfer_Encoding_Token,
                  Value => "8bit");
            else
               A.Headers.Add
                 (Name  => AWS.Messages.Content_Transfer_Encoding_Token,
                  Value => "binary");
            end if;
         end if;
      end if;

      if Data.Encode = Base64 then
         A.Headers.Update
           (Name  => AWS.Messages.Content_Transfer_Encoding_Token,
            Value => "base64");
      end if;

      if Data.Content_Id /= Null_Unbounded_String then
         A.Headers.Add
           (Name  => AWS.Messages.Content_Id_Token,
            Value => '<' & To_String (Data.Content_Id) & '>');
      end if;

      Attachments.Vector.Append (A);
   end Add;

   procedure Add
     (Parts : in out Alternatives;
      Data  : Content) is
   begin
      Parts.Parts.Append (Data);
   end Add;

   procedure Add
     (Attachments : in out List;
      Parts       : Alternatives) is
   begin
      Attachments.Vector.Append (Element (Parts));
   end Add;

   ----------------
   -- Content_Id --
   ----------------

   function Content_Id (Attachment : Element) return String is
   begin
      return AWS.Headers.Get (Attachment.Headers, Messages.Content_Id_Token);
   end Content_Id;

   ------------------
   -- Content_Type --
   ------------------

   function Content_Type (Attachment : Element) return String is
   begin
      return AWS.Headers.Values.Get_Unnamed_Value
        (AWS.Headers.Get (Attachment.Headers, Messages.Content_Type_Token));
   end Content_Type;

   -----------
   -- Count --
   -----------

   function Count (Attachments : List) return Natural is
   begin
      return Natural (Attachment_Table.Length (Attachments.Vector));
   end Count;

   ----------
   -- File --
   ----------

   function File
     (Filename     : String;
      Encode       : Encoding := None;
      Content_Id   : String := "";
      Content_Type : String := MIME.Text_Plain) return Content is
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

   function Filename (Attachment : Element) return String is
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

   procedure For_Every_Attachment (Attachments : List) is
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
     (Attachments : List;
      Index       : Positive) return Element is
   begin
      return Attachment_Table.Element
        (Container => Attachments.Vector, Index => Index);
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Attachments : List;
      Content_Id  : String) return Element
   is

      function Get_CID (Content_Id : String) return String;
      --  Returns Content_Id stripped from possibly surrounding
      --  '<' '>' and prefixing "cid:".

      -------------
      -- Get_CID --
      -------------

      function Get_CID (Content_Id : String) return String is
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

   function Headers (Attachment : Element) return AWS.Headers.List is
   begin
      return Attachment.Headers;
   end Headers;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Attachments : List;
      Process     : not null access procedure (Attachment : Element)) is
   begin
      for Position in Attachments.Vector.Iterate loop
         Attachment_Table.Query_Element (Position, Process);
      end loop;
   end Iterate;

   ----------
   -- Kind --
   ----------

   function Kind (Attachment : Element) return Attachment_Kind is
   begin
      return Attachment.Kind;
   end Kind;

   ------------
   -- Length --
   ------------

   function Length
     (Attachments : List;
      Boundary    : String) return Positive
   is
      L : Positive;
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

   function Local_Filename (Attachment : Element) return String is
   begin
      return To_String (Attachment.Data.Filename);
   end Local_Filename;

   -----------
   -- Reset --
   -----------

   procedure Reset
     (Attachments  : in out List;
      Delete_Files : Boolean) is
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

   function Root_MIME (Attachments : List) return Root_MIME_Kind is
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
     (Socket      : AWS.Net.Socket_Type'Class;
      Attachments : List;
      Boundary    : String)
   is

      procedure Send_Attachment (Attachment : Element);
      --  Sends one Attachment, including the start boundary

      procedure Send_Content (Attachment : Element);
      --  Set an in-memory content

      procedure Send_Content (Data : Content);

      procedure Send_Alternative (Attachment : Element);
      --  Send an alternative part

      Pref_Suf : constant String := "--";
      --  The MIME boundary prefix and suffix

      Simple_Alternative : constant Boolean :=
                             Root_MIME (Attachments) = Multipart_Alternative;

      ----------------------
      -- Send_Alternative --
      ----------------------

      procedure Send_Alternative (Attachment : Element) is
         A_Boundary : Unbounded_String;
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

         --  Send alternatives

         for Part of Attachment.Parts loop
            Net.Buffered.Put_Line (Socket, Pref_Suf & To_String (A_Boundary));
            Net.Buffered.Put_Line
              (Socket, Messages.Content_Type (To_String (Part.Content_Type)));
            Net.Buffered.New_Line (Socket);

            Send_Content (Part);
         end loop;

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

      procedure Send_Attachment (Attachment : Element) is
      begin
         case Attachment.Kind is
            when Data        => Send_Content (Attachment);
            when Alternative => Send_Alternative (Attachment);
         end case;
      end Send_Attachment;

      ------------------
      -- Send_Content --
      ------------------

      procedure Send_Content (Attachment : Element) is
      begin
         --  Send multipart message start boundary

         Net.Buffered.Put_Line (Socket, Pref_Suf & Boundary);

         --  Send header

         AWS.Headers.Send_Header (Socket, Attachment.Headers);
         Net.Buffered.New_Line (Socket);

         Send_Content (Attachment.Data);
      end Send_Content;

      procedure Send_Content (Data : Content) is

         procedure Send_File;

         procedure Send_Content;

         ------------------
         -- Send_Content --
         ------------------

         procedure Send_Content is
            Content_Len : constant Positive := Length (Data.Content);

            procedure Send;
            --  Send standard content

            procedure Send_Base64;
            --  Send a base64 content

            ----------
            -- Send --
            ----------

            procedure Send is
               Chunk_Shift : constant := 1023;
               K           : Positive := 1;
               L           : Natural;
            begin
               loop
                  L := Integer'Min (K + Chunk_Shift, Content_Len);
                  Net.Buffered.Put (Socket, Slice (Data.Content, K, L));
                  exit when L = Content_Len;
                  K := L + 1;
               end loop;

               Net.Buffered.New_Line (Socket);
            end Send;

            -----------------
            -- Send_Base64 --
            -----------------

            procedure Send_Base64 is
               Chunk_Size  : constant := 60;
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
     (Socket      : Net.Socket_Type'Class;
      Attachments : List;
      Boundary    : out Unbounded_String;
      Alternative : Boolean := False)
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

   -----------
   -- Value --
   -----------

   function Value
     (Data         : Unbounded_String;
      Name         : String := "";
      Encode       : Encoding := None;
      Content_Id   : String := "";
      Content_Type : String := MIME.Text_Plain) return Content
   is
      CD : Unbounded_String;
   begin
      if Encode = Base64 then
         Translator.Base64_Encode (Data, CD);
      else
         CD := Data;
      end if;

      return Content'(Attachments.Data, Length (Data),
                      Content_Id   => +Content_Id,
                      Content_Type => +Content_Type,
                      Filename     => +Name,
                      Encode       => Encode,
                      Content      => CD);
   end Value;

end AWS.Attachments;
