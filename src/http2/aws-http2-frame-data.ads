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

with AWS.Response;
with AWS.Status;
with AWS.Utils;

package AWS.HTTP2.Frame.Data is

   use Ada.Strings.Unbounded;

   type Object is new Frame.Object with private;

   overriding function Is_Defined (Self : Object) return Boolean;

   function Read
     (Sock   : Net.Socket_Type'Class;
      Header : Frame.Object) return Object
     with Pre => Header.Is_Defined;
   --  Read a DATA frame from Sock return the corresponding object

   function Create
     (Stream_Id : HTTP2.Stream_Id;
      Content   : String) return Object
     with Pre  => Stream_Id > 0 and then Content'Length > 0,
          Post => Create'Result.Kind = K_Data;
   --  Create a DATA frame with given content and stream id

   function Create
     (Stream_Id  : HTTP2.Stream_Id;
      Content    : Utils.Stream_Element_Array_Access;
      End_Stream : Boolean) return Object
     with Pre  => Stream_Id > 0 and then Content'Length > 0,
          Post => Create'Result.Kind = K_Data;
   --  Create a DATA frame with given content and stream id

   overriding procedure Send_Payload
     (Self : Object; Sock : Net.Socket_Type'Class)
     with Pre => Self.Is_Defined;
   --  Send the payload data

   overriding function Validate
     (Self : Object; Settings : Connection.Object) return Error_Codes;

   function Payload (Self : Object) return Unbounded_String
     with Pre => Self.Is_Defined;
   --  Returns the payload content

   function Payload_Length (Self : Object) return Positive
     with Pre => Self.Is_Defined;
   --  Length of the data payload

   procedure Append (Self : Object; Status : in out AWS.Status.Data)
     with Pre => Self.Is_Defined;
   --  Append data to the Status

   procedure Append (Self : Object; Response : in out AWS.Response.Data)
     with Pre => Self.Is_Defined;
   --  Append data to the response

   overriding procedure Dump_Payload (Self : Object);

private

   use type Utils.Stream_Element_Array_Access;

   --  RFC-7540 6.1
   --
   --  +---------------+
   --  |Pad Length? (8)|
   --  +---------------+-----------------------------------------------+
   --  |                            Data (*)                         ...
   --  +---------------------------------------------------------------+
   --  |                           Padding (*)                       ...
   --  +---------------------------------------------------------------+

   type Data_View (With_Padding : Boolean := True) is record
      case With_Padding is
         when True  => Pad_Length : Padding;
         when False => null;
      end case;
   end record with Unchecked_Union;

   type Payload_View (Flat : Boolean := True) is record
      case Flat is
         when False => D : not null access Data_View;
         when True =>  S : Utils.Stream_Element_Array_Access;
      end case;
   end record with Unchecked_Union;

   type Payload_View_Access is access all Payload_View;

   type Object is new Frame.Object with record
      Data : Payload_View;
   end record;

   overriding procedure Release (Self : in out Object);

   overriding function Is_Defined (Self : Object) return Boolean is
     (Frame.Object (Self).Is_Defined and then Self.Data.S /= null);

   function Payload_Length (Self : Object) return Positive is
     (Self.Data.S'Length -
        (if Self.Has_Flag (Padded_Flag)
         then Positive (Self.Data.D.Pad_Length + Padding'Size / 8)
         else 0));

end AWS.HTTP2.Frame.Data;
