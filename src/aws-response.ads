------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2000                            --
--                               Pascal Obry                                --
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
with Ada.Streams;

with AWS.Status;
with AWS.Messages;

package AWS.Response is

   use Ada;

   --  ??? this package should certainly be rewritten using an OO design using
   --  a set of tagged objects

   type Data is private;

   type Data_Mode is (Message, File);

   function Build (Content_Type : in String;
                   Message_Body : in String;
                   Status_Code  : in Messages.Status_Code := Messages.S200)
                  return Data;

   function Build (Content_Type : in String;
                   Message_Body : in Streams.Stream_Element_Array;
                   Status_Code  : in Messages.Status_Code := Messages.S200)
                  return Data;

   function Authenticate (Realm : in String) return Data;

   function File (Content_Type : in String;
                  Filename     : in String) return Data;

   function Mode           (D : in Data) return Data_Mode;
   function Status_Code    (D : in Data) return Messages.Status_Code;
   function Content_Length (D : in Data) return Positive;
   function Content_Type   (D : in Data) return String;
   function Message_Body   (D : in Data) return String;
   function Realm          (D : in Data) return String;
   function Binary         (D : in Data) return Streams.Stream_Element_Array;

   type Callback is access function (Request : in Status.Data) return Data;

   Default_Handler : constant Callback := null;

private

   use Ada.Strings.Unbounded;

   type Stream_Element_Array_Access is access Streams.Stream_Element_Array;

   type Data is record
      Mode           : Data_Mode;
      Status_Code    : Messages.Status_Code;
      Content_Length : Natural;
      Content_Type   : Unbounded_String;
      Message_Body   : Unbounded_String;
      Realm          : Unbounded_String;
      Elements       : Stream_Element_Array_Access;
   end record;

end AWS.Response;
