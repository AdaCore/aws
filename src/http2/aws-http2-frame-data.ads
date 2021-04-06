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
     (Stream_Id   : HTTP2.Stream_Id;
      Content     : String) return Object
     with Pre => Stream_Id > 0 and then Content'Length > 0;
   --  Create a DATA frame with given content and stream id

   overriding procedure Send_Payload
     (Self : Object; Sock : Net.Socket_Type'Class)
     with Pre => Self.Is_Defined;
   --  Send the payload data

   function Payload (Self : Object) return Unbounded_String
     with Pre => Self.Is_Defined;
   --  Returns the payload content

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

   type Payload_View (Flat : Boolean := False) is record
      case Flat is
         when False => P : Padding;
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

end AWS.HTTP2.Frame.Data;
