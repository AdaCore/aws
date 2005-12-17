------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2004                            --
--                                ACT-Europe                                --
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

with Ada.Streams.Stream_IO;

with GNAT.OS_Lib;

with AWS.Headers.Set;
with AWS.Messages;
with AWS.MIME;
with AWS.Net.Buffered;
with AWS.OS_Lib;

package body AWS.Attachments is

   use Ada;
   use Ada.Strings.Unbounded;

   function Value (V : in String) return Unbounded_String;
   --  Returns V as an Unbounded_String if V is not the empty string
   --  otherwise it returns Null_Unbounded_String.

   ---------
   -- Add --
   ---------

   procedure Add
     (Attachments : in out List;
      Filename    : in     String;
      Content_Id  : in     String)
   is
      use type Attachment_Table.Vector;

      File_Size : Natural;
      Tmp       : Element;
   begin
      --  Get the file size first, this will raise an exception
      --  if the file is missing.

      File_Size := Natural (AWS.OS_Lib.File_Size (Filename));

      Tmp.Filename := Value (Filename);

      AWS.Headers.Set.Add
        (Headers => Tmp.Headers,
         Name    => AWS.Messages.Content_Type_Token,
         Value   => MIME.Content_Type (Filename));

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

      AWS.Headers.Set.Add
        (Headers => Tmp.Headers,
         Name    => AWS.Messages.Content_Id_Token,
         Value   => '<' & Content_Id & '>');

      Tmp.Total_Length := AWS.Headers.Length (Tmp.Headers) + File_Size;

      Attachment_Table.Append (Attachments.Vector, Tmp);
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add
     (Attachments : in out List;
      Filename    : in     String;
      Headers     : in     AWS.Headers.List)
   is
      File_Size : Natural;
   begin
      --  Get the file size first, this will raise an exception
      --  if the file is missing.

      File_Size := Natural (AWS.OS_Lib.File_Size (Filename));

      Attachment_Table.Append
        (Attachments.Vector,
         (Filename     => Value (Filename),
          Headers      => Headers,
          Total_Length => AWS.Headers.Length (Headers) + File_Size));
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
      return AWS.Headers.Get (Attachment.Headers, Messages.Content_Type_Token);
   end Content_Type;

   -----------
   -- Count --
   -----------

   function Count (Attachments : in List) return Natural is
   begin
      return Natural (Attachment_Table.Length (Attachments.Vector));
   end Count;

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
      Content_Id  : in String)
      return Element
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

   ------------
   -- Length --
   ------------

   function Length
     (Attachments : in List;
      Boundary    : in String)
      return Natural
   is
      L : Natural;
   begin
      --  The length of all start boundaries, and the 2 characters longer
      --  end boundary.

      L := (Boundary'Length + 6) * (Count (Attachments) + 1) + 2;

      --  Add all content lengths

      for J in 1 .. Count (Attachments) loop
         L := L + Get (Attachments, J).Total_Length;
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
      Delete_Files : in     Boolean)
   is
      Dummy : Boolean;
   begin
      if Delete_Files then
         for J in 1 .. Count (Attachments) loop
            GNAT.OS_Lib.Delete_File
              (Name    => Local_Filename (Get (Attachments, J)),
               Success => Dummy);
         end loop;
      end if;
      Attachment_Table.Clear (Attachments.Vector);
   end Reset;

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

      Pref_Suf : constant String := "--";
      --  The MIME boundary prefix and suffix

      ---------------------
      -- Send_Attachment --
      ---------------------

      procedure Send_Attachment (Attachment : in Element) is
         Buffer : Streams.Stream_Element_Array (1 .. 4_096);
         Last   : Streams.Stream_Element_Offset;
         File   : Streams.Stream_IO.File_Type;
      begin
         --  Send multipart message start boundary

         Net.Buffered.New_Line (Socket);

         Net.Buffered.Put_Line (Socket, Pref_Suf & Boundary);

         --  Send header

         AWS.Headers.Send_Header (Socket, Attachment.Headers);

         Net.Buffered.New_Line (Socket);

         --  Send file content

         Streams.Stream_IO.Open
           (File, Streams.Stream_IO.In_File, To_String (Attachment.Filename));

         while not Streams.Stream_IO.End_Of_File (File) loop
            Streams.Stream_IO.Read (File, Buffer, Last);
            Net.Buffered.Write (Socket, Buffer (1 .. Last));
         end loop;

         Streams.Stream_IO.Close (File);

      exception
         when Net.Socket_Error =>
            --  Properly close the file if needed
            if Streams.Stream_IO.Is_Open (File) then
               Streams.Stream_IO.Close (File);
            end if;
            raise;
      end Send_Attachment;

   begin
      --  Send the attachments

      for J in
        1 .. Integer (Attachment_Table.Length (Attachments.Vector))
      loop
         Send_Attachment (Attachment_Table.Element
                            (Container => Attachments.Vector,
                             Index     => J));
      end loop;

      --  Send multipart message end boundary

      Net.Buffered.New_Line (Socket);

      Net.Buffered.Put_Line (Socket, Pref_Suf & Boundary & Pref_Suf);
   end Send;

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
