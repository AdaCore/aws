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
     (Filename     : String;
      Encode       : Encoding := None;
      Content_Id   : String := "";
      Content_Type : String := MIME.Text_Plain) return Content;
   --  A filename as content, if Encode is set to Base64 the file content will
   --  be base64 encoded.

   function Value
     (Data         : Unbounded_String;
      Name         : String := "";
      Encode       : Encoding := None;
      Content_Id   : String := "";
      Content_Type : String := MIME.Text_Plain) return Content;
   --  An unbounded string as content

   function Value
     (Data         : String;
      Name         : String := "";
      Encode       : Encoding := None;
      Content_Id   : String := "";
      Content_Type : String := MIME.Text_Plain) return Content
   is (Value (To_Unbounded_String (Data), Name, Encode, Content_Id,
              Content_Type));
   --  A string as content

   type Attachment_Kind is (Data, Alternative);
   --  Data        : for a standard MIME attachment
   --  Alternative : for a set of alternative content

   procedure Add
     (Attachments : in out List;
      Filename    : String;
      Content_Id  : String;
      Headers     : AWS.Headers.List := AWS.Headers.Empty_List;
      Name        : String := "";
      Encode      : Encoding := None)
   with Post => Count (Attachments) = Count (Attachments'Old) + 1;
   --  Adds an Attachment to the list.
   --  Note that the encoding will overwrite the corresponding entry in
   --  headers.

   procedure Add
     (Attachments : in out List;
      Filename    : String;
      Headers     : AWS.Headers.List;
      Name        : String := "";
      Encode      : Encoding := None)
   with Post => Count (Attachments) = Count (Attachments'Old) + 1;
   --  Adds an Attachment to the list.
   --  Note that the encoding will overwrite the corresponding entry in
   --  headers.

   procedure Add
     (Attachments : in out List;
      Name        : String;
      Data        : Content;
      Headers     : AWS.Headers.List := AWS.Headers.Empty_List)
   with Post => Count (Attachments) = Count (Attachments'Old) + 1;
   --  Adds an Attachment to the list.
   --  Note that the encoding and content type attached to Data will
   --  overwrite the corresponding entry in headers.

   --  Alternatives content

   type Alternatives is private;

   procedure Add
     (Parts : in out Alternatives;
      Data  : Content);
   --  Add an alternative content

   procedure Add
     (Attachments : in out List;
      Parts       : Alternatives);
   --  Add an alternative group to the current attachment list

   procedure Reset
     (Attachments  : in out List;
      Delete_Files : Boolean)
   with Post => Count (Attachments) = 0;
   --  Reset the list to be empty. If Delete_Files is set to true the
   --  attached files are removed from the file system.

   function Count (Attachments : List) return Natural with Inline;
   --  Returns the number of Attachments in the data

   function Get
     (Attachments : List;
      Index       : Positive) return Element
   with Pre => Index <= Count (Attachments);
   --  Returns specified Attachment

   function Get
     (Attachments : List;
      Content_Id  : String) return Element
   with
     Pre =>
       (for some K in 1 .. Count (Attachments)
        => AWS.Attachments.Content_Id (Get (Attachments, K)) = Content_Id);
   --  Returns the Attachment with the Content Id

   generic
      with procedure Action
        (Attachment : Element;
         Index      : Positive;
         Quit       : in out Boolean);
   procedure For_Every_Attachment (Attachments : List);
   --  Calls action for every Attachment in Message. Stop iterator if Quit is
   --  set to True, Quit is set to False by default.

   procedure Iterate
     (Attachments : List;
      Process     : not null access procedure (Attachment : Element));
   --  Calls Process for every Attachment in Message

   function Headers (Attachment : Element) return AWS.Headers.List with Inline;
   --  Returns the list of header lines for the attachment

   function Content_Type (Attachment : Element) return String;
   --  Get value for "Content-Type:" header

   function Content_Id (Attachment : Element) return String;
   --  Returns Attachment's content id

   function Local_Filename (Attachment : Element) return String;
   --  Returns the local filename of the Attachment.
   --  Local filename is the name the receiver used when extracting the
   --  Attachment into a file.

   function Filename (Attachment : Element) return String;
   --  Original filename on the server side. This is generally encoded on the
   --  content-type or content-disposition header.

   function Kind (Attachment : Element) return Attachment_Kind with Inline;
   --  Returns the kind of the given attachment

   function Length
     (Attachments : List;
      Boundary    : String) return Positive
   with Post => Length'Result > 8;
   --  Returns the complete size of all attachments including the surrounding
   --  boundaries.

   procedure Send_MIME_Header
     (Socket      : Net.Socket_Type'Class;
      Attachments : List;
      Boundary    : out Unbounded_String;
      Alternative : Boolean := False);
   --  Output MIME header, returns the boundary for the content

   procedure Send
     (Socket      : AWS.Net.Socket_Type'Class;
      Attachments : List;
      Boundary    : String);
   --  Send all Attachments, including the surrounding boundarys, in the list
   --  to the socket.

   type Root_MIME_Kind is (Multipart_Mixed, Multipart_Alternative);

   function Root_MIME (Attachments : List) return Root_MIME_Kind;
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
