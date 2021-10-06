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

with AWS.Headers;
with AWS.HTTP2.Connection;
with AWS.HTTP2.HPACK.Table;

package AWS.HTTP2.Frame.Continuation is

   type Object is new Frame.Object with private;

   function Read
     (Sock   : Net.Socket_Type'Class;
      Header : Frame.Object) return Object;

   function Create
     (Table       : not null access HTTP2.HPACK.Table.Object;
      Settings    : not null access Connection.Object;
      Stream_Id   : HTTP2.Stream_Id;
      List        : Headers.List;
      End_Headers : Boolean := True) return Object
     with Pre  => Stream_Id /= 0,
          Post => (if End_Headers then Create'Result.Flags = End_Headers_Flag)
                  and then Create'Result.Kind = K_Continuation;
   --  Create a CONTINUATION frame with given content

   overriding procedure Send_Payload
     (Self : Object; Sock : Net.Socket_Type'Class);

   overriding function Validate
     (Self : Object; Settings : Connection.Object) return Error_Codes;

   --  Iterator interface

   function Content_Length
     (Self : Object) return Stream_Element_Count
     with Pre => Self.Is_Defined;

   function Get
     (Self  : Object;
      Index : Stream_Element_Offset) return Stream_Element
     with Pre => Self.Is_Defined;

private

   --  RFC-7540 6.10
   --
   --  +-+-------------+-----------------------------------------------+
   --  |                   Header Block Fragment (*)                 ...
   --  +---------------------------------------------------------------+

   type Payload_View is record
      S : Utils.Stream_Element_Array_Access;
   end record;

   type Object is new Frame.Object with record
      Data : Payload_View;
   end record;

   overriding procedure Release (Self : in out Object);

end AWS.HTTP2.Frame.Continuation;
