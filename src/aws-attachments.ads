------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2008, AdaCore                     --
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

with Ada.Strings.Unbounded;

with AWS.Headers;
with AWS.MIME;
with AWS.Net;

private with Ada.Containers.Vectors;

package AWS.Attachments is

   use Ada.Strings.Unbounded;

   type Element is private;
   type List is tagged private;

   Empty_List : constant List;

   type Content is private;

   type Encoding is (None, Base64);

   function File
     (Filename     : in String;
      Encode       : in Encoding := None;
      Content_Id   : in String := "";
      Content_Type : in String := MIME.Text_Plain) return Content;
   --  A filename as content, if Encode is set to Base64 the file content will
   --  be base64 encoded.

   function Value
     (Data         : in String;
      Name         : in String := "";
      Encode       : in Encoding := None;
      Content_Id   : in String := "";
      Content_Type : in String := MIME.Text_Plain) return Content;
   --  A string as content

   type Attachment_Kind is (Data, Alternative);
   --  Data        : for a standard MIME attachment
   --  Alternative : for a set of alternative content

   procedure Add
     (Attachments : in out List;
      Filename    : in     String;
      Content_Id  : in     String;
      Encode      : in     Encoding := None);
   --  Adds an Attachment to the list. The header of the Attachment is
   --  generated.

   procedure Add
     (Attachments : in out List;
      Filename    : in     String;
      Headers     : in     AWS.Headers.List;
      Encode      : in     Encoding := None);
   --  Adds an Attachment to the list

   procedure Add
     (Attachments : in out List;
      Data        : in     Content;
      Headers     : in     AWS.Headers.List := AWS.Headers.Empty_List);
   --  Add content to the attachment list

   --  Alternatives content

   type Alternatives is private;

   procedure Add
     (Parts : in out Alternatives;
      Data  : in     Content);
   --  Add an alternative content

   procedure Add
     (Attachments : in out List;
      Parts       : in     Alternatives);
   --  Add an alternative group to the current attachment list

   procedure Reset
     (Attachments  : in out List;
      Delete_Files : in     Boolean);
   --  Reset the list to be empty. If Delete_Files is set to true the
   --  attached files are removed from the file system.

   function Count (Attachments : in List) return Natural;
   pragma Inline (Count);
   --  Returns the number of Attachments in the data

   function Get
     (Attachments : in List;
      Index       : in Positive) return Element;
   --  Returns specified Attachment

   function Get
     (Attachments : in List;
      Content_Id  : in String) return Element;
   --  Returns the Attachment with the Content Id

   generic
      with procedure Action
        (Attachment : in     Element;
         Index      : in     Positive;
         Quit       : in out Boolean);
   procedure For_Every_Attachment (Attachments : in List);
   --  Calls action for every Attachment in Message. Stop iterator if Quit is
   --  set to True, Quit is set to False by default.

   procedure Iterate
     (Attachments : in List;
      Process     : access procedure (Attachment : in Element));
   --  Calls Process for every Attachment in Message

   function Headers (Attachment : in Element) return AWS.Headers.List;
   pragma Inline (Headers);
   --  Returns the list of header lines for the attachment

   function Content_Type (Attachment : in Element) return String;
   --  Get value for "Content-Type:" header

   function Content_Id (Attachment : in Element) return String;
   --  Returns Attachment's content id

   function Local_Filename (Attachment : in Element) return String;
   --  Returns the local filename of the Attachment.
   --  Local filename is the name the receiver used when extracting the
   --  Attachment into a file.

   function Filename (Attachment : in Element) return String;
   --  Original filename on the server side. This is generally encoded on the
   --  content-type or content-disposition header.

   function Kind (Attachment : in Element) return Attachment_Kind;
   pragma Inline (Kind);
   --  Returns the kind of the given attachment

   function Length
     (Attachments : in List;
      Boundary    : in String) return Natural;
   --  Returns the complete size of all attachments including the surrounding
   --  boundaries.

   procedure Send_MIME_Header
     (Socket      : in     Net.Socket_Type'Class;
      Attachments : in     List;
      Boundary    :    out Unbounded_String;
      Alternative : in     Boolean := False);
   --  Output MIME header, returns the boundary for the content

   procedure Send
     (Socket      : in AWS.Net.Socket_Type'Class;
      Attachments : in List;
      Boundary    : in String);
   --  Send all Attachments, including the surrounding boundarys, in the list
   --  to the socket.

   type Root_MIME_Kind is (Multipart_Mixed, Multipart_Alternative);

   function Root_MIME (Attachments : in List) return Root_MIME_Kind;
   --  Returns the root MIME kind for the given attachment list

private

   use Ada;

   type Content_Kind is (File, Data);

   type Content (Kind : Content_Kind := File) is record
      Length       : Natural;
      Content_Id   : Unbounded_String;
      Content_Type : Unbounded_String;
      Filename     : Unbounded_String;
      Encode       : Encoding;

      case Kind is
         when File =>
            null;
         when Data =>
            Content : Unbounded_String;
      end case;
   end record;

   package Alternative_Table is new Containers.Vectors (Positive, Content);

   type Element (Kind : Attachment_Kind := Data) is record
      Headers      : AWS.Headers.List;
      Total_Length : Natural;

      case Kind is
         when Data =>
            Data : Content;
         when Alternative =>
            Parts : Alternative_Table.Vector;
      end case;
   end record;

   type Alternatives is new Element (Kind => Alternative);

   package Attachment_Table is new Ada.Containers.Vectors (Positive, Element);

   type List is tagged record
      Vector : Attachment_Table.Vector;
   end record;

   Empty_List : constant List := (Vector => Attachment_Table.Empty_Vector);

end AWS.Attachments;
