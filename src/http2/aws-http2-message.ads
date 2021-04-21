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

package AWS.HTTP2.Message is

   use Ada.Strings.Unbounded;

   use type AWS.HTTP2.Frame.List.Count_Type;
   use type Response.Data_Mode;

   type Object (Mode : Response.Data_Mode) is tagged limited private;

   function Is_Defined (Self : Object) return Boolean;

   function Create
     (Headers : AWS.Headers.List;
      Payload : Unbounded_String)
      return Object
     with Pre  => Length (Payload) > 0 or else not Headers.Is_Empty,
          Post => Create'Result.Is_Defined;
   --  Create a message based on the given headers and payload

   function Create
     (Headers  : AWS.Headers.List;
      Filename : String)
      return Object
     with Pre  => not Headers.Is_Empty,
          Post => Create'Result.Is_Defined;
   --  Create a message based on the given headers and payload

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

   function To_Frames
     (Self      : Object;
      Ctx       : in out Server.Context.Object;
      Stream_Id : HTTP2.Stream_Id) return AWS.HTTP2.Frame.List.Object
     with Pre  => Self.Is_Defined,
          Post => To_Frames'Result.Length > 0
                    and then
                  (for all P of To_Frames'Result =>
                     P.Is_Defined and then P.Stream_Id = Stream_Id);
   --  Get frames for this message. The headers is returned as an HEADERS frame
   --  and the payload as a DATA frame.

private

   type Object (Mode : Response.Data_Mode) is tagged limited record
      Headers : AWS.Headers.List;

      case Mode is
         when Response.Message =>
            Payload  : Unbounded_String;

         when Response.File | Response.File_Once =>
            Filename : Unbounded_String;

         when Response.No_Data =>
            null;

         when others =>
            null;
      end case;
   end record;

   function Headers (Self : Object) return AWS.Headers.List is (Self.Headers);

   function Payload (Self : Object) return Unbounded_String is (Self.Payload);

   function Filename (Self : Object) return String is
     (To_String (Self.Filename));

   function Is_Defined (Self : Object) return Boolean is
     (Self.Mode /= Response.No_Data);

end AWS.HTTP2.Message;
