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

with System;

package AWS.HTTP2.Frame.Window_Update is

   type Object is new Frame.Object with private;

   Default_Window_Size : constant := 65_535;
   --  The default window size for new streams

   overriding function Is_Defined (Self : Object) return Boolean;

   subtype Size_Increment_Type is Byte_4 range 0 .. 2 ** 31 - 1;

   function Read
     (Sock   : Net.Socket_Type'Class;
      Header : Frame.Object) return Object
     with Pre  => Header.Is_Defined,
          Post => Read'Result.Kind = K_Window_Update;
   --  Read a WINDOW_UPDATE frame from Sock return the corresponding object

   function Create
     (Stream_Id      : HTTP2.Stream_Id;
      Size_Increment : Size_Increment_Type) return Object
     with Post => Create'Result.Kind = K_Window_Update;
   --  Create a WINDOW_UPDATE frame out of the set of settings

   overriding procedure Send_Payload
     (Self : Object; Sock : Net.Socket_Type'Class)
     with Pre => Self.Is_Defined;
   --  Send frame content to Sock

   overriding function Validate
     (Self : Object; Settings : Connection.Object) return Error_Codes
     with Pre => Self.Is_Defined;

   function Size_Increment (Self : Object) return Size_Increment_Type
     with Pre => Self.Is_Defined;
   --  Returns the Windows size increment

   overriding procedure Dump_Payload (O : Object);

private

   --  RFC-7540 6.9
   --
   --  +-+-------------------------------------------------------------+
   --  |R|              Window Size Increment (31)                     |
   --  +-+-------------------------------------------------------------+

   type Payload is record
      R              : Bit_1               := 0;
      Size_Increment : Size_Increment_Type := 0;
   end record;

   for Payload'Bit_Order use System.High_Order_First;
   for Payload'Scalar_Storage_Order use System.High_Order_First;
   for Payload use record
      R              at 0 range 0 .. 0;
      Size_Increment at 0 range 1 .. 31;
   end record;

   type Payload_View (Flat : Boolean := False) is record
      case Flat is
         when False => P : Payload;
         when True =>  S : Stream_Element_Array (1 .. Payload'Size / 8);
      end case;
   end record with Unchecked_Union;

   type Object is new Frame.Object with record
      Data : Payload_View;
   end record;

   overriding function Is_Defined (Self : Object) return Boolean is
     (Frame.Object (Self).Is_Defined and then Self.Data.P.Size_Increment /= 0);

   function Size_Increment (Self : Object) return Size_Increment_Type is
     (Self.Data.P.Size_Increment);

end AWS.HTTP2.Frame.Window_Update;
