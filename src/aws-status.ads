------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimov - Pascal Obry                                 --
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

--  This package is used to keep the HTTP protocol status. Client can then
--  request the status for various value like the requested URI and the
--  Content_Length.

with Ada.Strings.Unbounded;
with Ada.Streams;

with AWS.Session;
with AWS.Parameters;
with Sockets;

package AWS.Status is

   type Data is private;
   --  Parameters_Case_Sensitive indicate if the parameters name are to be
   --  handled with case sensitivity or not. Default is True.

   type Request_Method is (GET, HEAD, POST, PUT);

   subtype Socket_Type is Sockets.Socket_FD'Class;

   type Socket_Access is access all Socket_Type;

   function Authorization_Name     (D : in Data) return String;
   function Authorization_Password (D : in Data) return String;
   function Connection             (D : in Data) return String;
   function Content_Length         (D : in Data) return Natural;
   function Content_Type           (D : in Data) return String;
   function File_Up_To_Date        (D : in Data) return Boolean;
   function Has_Session            (D : in Data) return Boolean;
   function Host                   (D : in Data) return String;
   function HTTP_Version           (D : in Data) return String;
   function If_Modified_Since      (D : in Data) return String;
   function Method                 (D : in Data) return Request_Method;
   function Multipart_Boundary     (D : in Data) return String;
   function Parameters             (D : in Data) return Parameters.List;
   function Peername               (D : in Data) return String;
   function Session                (D : in Data) return String;
   function Session                (D : in Data) return AWS.Session.ID;
   function URI                    (D : in Data) return String;
   function Socket                 (D : in Data) return Socket_Type;

   subtype Stream_Element_Array is Ada.Streams.Stream_Element_Array;

   function Binary_Data (D : in Data) return Stream_Element_Array;
   --  Returns the binary data message content.

private

   pragma Inline (Authorization_Name);
   pragma Inline (Authorization_Password);
   pragma Inline (Host);
   pragma Inline (Method);
   pragma Inline (URI);
   pragma Inline (HTTP_Version);
   pragma Inline (Socket);

   use Ada.Strings.Unbounded;

   type Stream_Element_Array_Access is access Stream_Element_Array;

   type Data is record
      Connection        : Unbounded_String;
      Host              : Unbounded_String;
      Peername          : Unbounded_String;
      Method            : Request_Method     := GET;
      URI               : Unbounded_String;
      Parameters        : AWS.Parameters.List;
      Binary_Data       : Stream_Element_Array_Access := null;
      HTTP_Version      : Unbounded_String;
      Content_Type      : Unbounded_String;
      Boundary          : Unbounded_String;
      Content_Length    : Natural            := 0;
      If_Modified_Since : Unbounded_String;
      File_Up_To_Date   : Boolean            := False;
      Socket            : Socket_Access;
      Auth_Name         : Unbounded_String;
      Auth_Password     : Unbounded_String;
      Session_ID        : Unbounded_String;
   end record;

end AWS.Status;
