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

with Ada.Strings.Unbounded;

with AWS.Headers;
with AWS.HTTP2.Frame.List;
with AWS.Response;
with AWS.Server.Context;

private with AWS.Utils;

limited with AWS.HTTP2.Stream;

package AWS.HTTP2.Message is

   use Ada.Strings.Unbounded;

   use type AWS.HTTP2.Frame.List.Count_Type;
   use type Response.Data_Mode;

   type Object (Mode : Response.Data_Mode) is tagged private;

   function Is_Defined (Self : Object) return Boolean;

   function Create
     (Answer    : Response.Data;
      Stream_Id : HTTP2.Stream_Id) return Object
     with Post => Create'Result.Is_Defined;

   function Create
     (Headers   : AWS.Headers.List;
      Payload   : Unbounded_String;
      Stream_Id : HTTP2.Stream_Id)
      return Object
     with Pre  => Length (Payload) > 0 or else not Headers.Is_Empty,
          Post => Create'Result.Is_Defined;
   --  Create a message based on the given headers and payload

   function Create
     (Headers  : AWS.Headers.List;
      Filename : String;
      Stream_Id : HTTP2.Stream_Id)
      return Object
     with Pre  => not Headers.Is_Empty,
          Post => Create'Result.Is_Defined;
   --  Create a message based on the given headers and payload

   function Stream_Id (Self : Object) return HTTP2.Stream_Id
     with Pre => Self.Is_Defined;

   function Headers (Self : Object) return AWS.Headers.List
     with Pre => Self.Is_Defined;
   --  Get the headers for this message

   function Payload (Self : Object) return Unbounded_String
     with Pre => Self.Is_Defined and then Self.Mode = Response.Message;
   --  Get the payload for this message

   function Filename (Self : Object) return String
     with Pre => Self.Is_Defined
                 and then Self.Mode in Response.File .. Response.File_Once;
   --  Get the filename for this message

   function Has_Body (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if message has a body

   function To_Frames
     (Self   : in out Object;
      Ctx    : in out Server.Context.Object;
      Stream : HTTP2.Stream.Object) return AWS.HTTP2.Frame.List.Object
     with Pre  => Self.Is_Defined,
          Post => To_Frames'Result.Length > 0
                    and then
                  (for all P of To_Frames'Result => P.Is_Defined);
   --  Get frames for this message. The headers is returned as an HEADERS frame
   --  and the payload as a DATA frame. The list may not contain the while data
   --  payload depending on the current flow control window. If some more data
   --  is available, More_Frame below will return true.

   function More_Frames (Self : Object) return Boolean;
   --  Returns True if some more data are available and so should be sent

private

   use type Utils.File_Size_Type;

   type Object (Mode : Response.Data_Mode) is tagged record
      Stream_Id : HTTP2.Stream_Id;
      Headers   : AWS.Headers.List;
      Sent      : Utils.File_Size_Type := 0;
      Length    : Utils.File_Size_Type := 0;
      H_Sent    : Boolean := False; -- Whether the header has been sent

      case Mode is
         when Response.Message =>
            Payload  : Unbounded_String;

         when Response.File | Response.File_Once =>
            Filename : Unbounded_String;

         when others =>
            null;
      end case;
   end record;

   function Headers (Self : Object) return AWS.Headers.List is (Self.Headers);

   function Has_Body (Self : Object) return Boolean is
     (Self.Mode in Response.Message | Response.File | Response.File_Once);

   function Payload (Self : Object) return Unbounded_String is (Self.Payload);

   function Filename (Self : Object) return String is
     (To_String (Self.Filename));

   function Is_Defined (Self : Object) return Boolean is
     (Self.Mode /= Response.No_Data);

   function Stream_Id (Self : Object) return HTTP2.Stream_Id is
     (Self.Stream_Id);

   function More_Frames (Self : Object) return Boolean is
      (Self.Sent < Self.Length);

end AWS.HTTP2.Message;
