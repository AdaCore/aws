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

package AWS.HTTP2.Frame.Ping is

   type Object is new Frame.Object with private;

   function Read
     (Sock   : Net.Socket_Type'Class;
      Header : Frame.Object) return Object
     with Pre => Header.Is_Defined;
   --  Read a PING frame from Sock return the corresponding object

   type Opaque_Data is new Stream_Element_Array (1 .. 8);

   Default_Data : constant Opaque_Data;

   function Create
     (Data  : Opaque_Data := Default_Data;
      Flags : Flags_Type := 0) return Object
     with Post => Create'Result.Is_Defined
                  and then Create'Result.Kind = K_Ping;
   --  Create a PING frame (stream id is always 0)

   overriding procedure Send_Payload
     (Self : Object; Sock : Net.Socket_Type'Class);
   --  Send payload content

   overriding function Validate
     (Self : Object; Settings : Connection.Object) return Error_Codes;

private

   Default_Data : constant Opaque_Data := (0, 0, 0, 0, 0, 0, 0, 0);

   --  RFC-7540 6.7
   --
   --  +---------------------------------------------------------------+
   --  |                      Opaque Data (64)                         |
   --  +---------------------------------------------------------------+

   type Object is new Frame.Object with record
      Data : Opaque_Data;
   end record;

end AWS.HTTP2.Frame.Ping;
