------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2004-2005                          --
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

--  $Id$

with Ada.Strings.Unbounded;
with AI302.Containers.Indefinite_Vectors;

with AWS.Headers;
with AWS.Net;

package AWS.Attachments is

   type Element is private;
   type List is tagged private;

   Empty_List : constant List;

   procedure Add
     (Attachments : in out List;
      Filename    : in     String;
      Content_Id  : in     String);
   --  Adds an Attachment to the list. The header of the Attachment is
   --  generated.

   procedure Add
     (Attachments : in out List;
      Filename    : in     String;
      Headers     : in     AWS.Headers.List);
   --  Adds an Attachment to the list

   procedure Reset
     (Attachments  : in out List;
      Delete_Files : in     Boolean);
   --  Reset the list to be empty. If Delete_Files is set to true the
   --  attached files are removed from the file system.

   function Count (Attachments : in List) return Natural;
   --  Returns the number of Attachments in the data

   function Get
     (Attachments : in List;
      Index       : in Positive)
      return Element;
   --  Returns specified Attachment

   function Get
     (Attachments : in List;
      Content_Id  : in String)
      return Element;
   --  Returns the Attachment with the Content Id

   generic
      with procedure Action
        (Attachment : in     Element;
         Index      : in     Positive;
         Quit       : in out Boolean);
   procedure For_Every_Attachment (Attachments : in List);
   --  Calls action for every Attachment in Message. Stop iterator if Quit is
   --  set to True, Quit is set to False by default.

   function Headers (Attachment : in Element) return AWS.Headers.List;
   --  Returns the list of header lines for the attachment

   function Content_Type (Attachment : in Element) return String;
   --  Get value for "Content-Type:" header

   function Content_Id (Attachment : in Element) return String;
   --  Returns Attachment's content id

   function Local_Filename (Attachment : in Element) return String;
   --  Returns the local filename of the Attachment.
   --  Local filename is the name the receiver used when extracting the
   --  Attachment into a file.

   function Length
     (Attachments : in List;
      Boundary    : in String) return Natural;
   --  Returns the complete size of all attachments including the surrounding
   --  boundaries.

   procedure Send
     (Socket      : in AWS.Net.Socket_Type'Class;
      Attachments : in List;
      Boundary    : in String);
   --  Send all Attachments, including the surrounding boundarys, in the list
   --  to the socket

private

   type Element is record
      Headers      : AWS.Headers.List;
      Filename     : Ada.Strings.Unbounded.Unbounded_String;
      Total_Length : Natural;
   end record;

   package Attachment_Table is
     new AI302.Containers.Indefinite_Vectors (Positive, Element);

   type List is tagged record
      Vector : Attachment_Table.Vector;
   end record;

   Empty_List : constant List := (Vector => Attachment_Table.Empty_Vector);

end AWS.Attachments;
