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

package AWS.HTTP2.Message is

   use Ada.Strings.Unbounded;
   use type AWS.HTTP2.Frame.List.Count_Type;

   type Object is tagged private;

   Undefined : constant Object;

   function Is_Defined (Self : Object) return Boolean;

   function Create
     (Headers : AWS.Headers.List;
      Payload : Unbounded_String)
      return Object
     with Pre  => Length (Payload) > 0 or else not Headers.Is_Empty,
          Post => Create'Result.Is_Defined;
   --  Create a message based on the given headers and payload

   function Headers (Self : Object) return AWS.Headers.List
     with Pre => Self.Is_Defined;
   --  Get the headers for this message

   function Payload (Self : Object) return Unbounded_String
     with Pre => Self.Is_Defined;
   --  Get the payload for this message

   function To_Frames
     (Self      : Object;
      Ctx       : in out Context;
      Stream_Id : HTTP2.Stream_Id) return AWS.HTTP2.Frame.List.Object
     with Pre  => Self.Is_Defined,
          Post => To_Frames'Result.Length > 0
                    and then
                  (for all P of To_Frames'Result =>
                     P.Is_Defined and then P.Stream_Id = Stream_Id);
   --  Get frames for this message. The headers is returned as an HEADERS frame
   --  and the payload as a DATA frame.

private

   type Object is tagged record
      Headers : AWS.Headers.List;
      Payload : Unbounded_String;
   end record;

   Undefined : constant Object :=
                 (AWS.Headers.Empty_List, Null_Unbounded_String);

   function Headers (Self : Object) return AWS.Headers.List is (Self.Headers);

   function Payload (Self : Object) return Unbounded_String is (Self.Payload);

   function Is_Defined (Self : Object) return Boolean is (Self /= Undefined);

end AWS.HTTP2.Message;
