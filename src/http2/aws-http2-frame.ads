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

with Ada.Finalization;

with System;

with AWS.Net;

limited with AWS.HTTP2.Connection;

private with AWS.Utils;

package AWS.HTTP2.Frame is

   use Ada;

   type Object is new Finalization.Controlled with private;

   type Kind_Type is (K_Data, K_Headers, K_Priority,
                      K_RST_Stream, K_Settings, K_Push_Promise,
                      K_Ping, K_GoAway, K_Window_Update,
                      K_Continuation, K_Invalid)
     with Size => 8;
   --  Frame kind, see section 6 RFC 7540

   type Flags_Type is mod 2 ** 8 with Size => 8;

   subtype Length_Type is Byte_3 range 0 .. 2 ** 24 - 1;

   End_Stream_Flag  : constant Flags_Type;
   Ack_Flag         : constant Flags_Type;
   End_Headers_Flag : constant Flags_Type;
   Padded_Flag      : constant Flags_Type;
   Priority_Flag    : constant Flags_Type;
   --  The fkags that can be attached to a frame

   function Is_Defined (Self : Object) return Boolean;
   --  Returns True is Self is defined

   function Read
     (Sock     : Net.Socket_Type'Class;
      Settings : Connection.Object) return Object'Class;
   --  Read a frame, the frame is composed of a standard header. The header
   --  kind is encoded into the header. The remaining of the frame is read
   --  by dedecated routines the child packegs.

   procedure Send
     (Self : Object'Class; Sock : Net.Socket_Type'Class)
     with Pre => Self.Is_Defined;
   --  Send header frame

   procedure Send_Payload
     (Self : Object; Sock : Net.Socket_Type'Class) is null
     with Pre'Class => Self.Is_Defined;
   --  Send frame payload, this default does nothing and must be overriden in
   --  child packages.

   function Kind (Self : Object) return Kind_Type
     with Pre => Self.Is_Defined;
   --  The frame's kind

   function Stream_Id (Self : Object) return HTTP2.Stream_Id
     with Pre => Self.Is_Defined;
   --  The frame's stream-id

   function Flags (Self : Object) return Flags_Type
     with Pre => Self.Is_Defined;
   --  The frams flags

   procedure Set_Flags (Self : in out Object; Flags : Flags_Type)
     with Pre => Self.Is_Defined;
   --  Set the frame's flags

   procedure Set_Flag
     (Self : in out Object; Flag : Flags_Type; Value : Boolean := True)
     with Pre => Self.Is_Defined;
   --  Set the frame's flag to Value

   function Has_Flag (Self : Object'Class; Flag : Flags_Type) return Boolean
     with Pre => Self.Is_Defined;
   --  Check if the frame as a Flag set

   function Length (Self : Object) return Length_Type
     with Pre => Self.Is_Defined;
   --  The length of the frame payload

   function Validate
     (Self     : Object;
      Settings : Connection.Object) return Error_Codes
     with Pre'Class => Self.Is_Defined;
   --  Validate the frame content and return an error code different than
   --  C_No_Error if the frame is malformed. This default implementation return
   --  C_No_Error and is supposed to be overriden by frame implementation. if
   --  needed.

   function Is_Valid
     (Self     : Object'Class;
      Settings : Connection.Object;
      Error    : out Error_Codes) return Boolean
     with Pre  => Self.Is_Defined,
          Post => Is_Valid'Result = (Error = C_No_Error);
   --  Set Error to appropriate value with Validate routine and return True if
   --  Error became C_No_Error, returns False otherwise.

   --  Debug oritented routines:

   procedure Dump (Self : Object'Class; Message : String);
   procedure Dump_Payload (Self : Object) is null;

private

   use type Utils.Counter_Access;

   End_Stream_Flag  : constant Flags_Type := 16#01#;
   Ack_Flag         : constant Flags_Type := 16#01#;
   End_Headers_Flag : constant Flags_Type := 16#04#;
   Padded_Flag      : constant Flags_Type := 16#08#;
   Priority_Flag    : constant Flags_Type := 16#20#;

   for Kind_Type use (K_Data          => 16#0#,
                      K_Headers       => 16#1#,
                      K_Priority      => 16#2#,
                      K_RST_Stream    => 16#3#,
                      K_Settings      => 16#4#,
                      K_Push_Promise  => 16#5#,
                      K_Ping          => 16#6#,
                      K_GoAway        => 16#7#,
                      K_Window_Update => 16#8#,
                      K_Continuation  => 16#9#,
                      K_Invalid       => 16#A#);

   type Header is record
      Length    : Length_Type     := 0;
      Kind      : Kind_Type       := K_Invalid;
      Flags     : Flags_Type      := 0;
      R         : Bit_1           := 0;
      Stream_Id : HTTP2.Stream_Id := 0;
   end record
     with Dynamic_Predicate =>
            (if Kind = K_Window_Update then Length = 4)
              and then
            (if Kind = K_Headers then Stream_Id > 0)
              and then
            --  6.5 frame size error:
            (if Kind = K_Settings and then Flags = Ack_Flag then Length = 0)
              and then
            --  6.5 frame size error:
            (if Kind = K_Settings then (Length mod 6) = 0)
              and then
            --  6.5 / 6.7 protocol error:
            (if Kind in K_Settings | K_Ping then Stream_Id = 0)
              and then
            --  6.1 protocol error:
            (if Kind = K_Data then Stream_Id /= 0)
              and then
            R = 0;

   --  +-----------------------------------------------+
   --  |                 Length (24)                   |
   --  +---------------+---------------+---------------+
   --  |   Type (8)    |   Flags (8)   |
   --  +-+-------------+---------------+-------------------------------+
   --  |R|                 Stream Identifier (31)                      |
   --  +=+=============================================================+
   --  |                   Frame Payload (0...)                      ...
   --  +---------------------------------------------------------------+

   for Header'Bit_Order use System.High_Order_First;
   for Header'Scalar_Storage_Order use System.High_Order_First;
   for Header use record
      Length    at 0 range 0 .. 23;
      Kind      at 0 range 24 .. 31;
      Flags     at 4 range 0 .. 7;
      R         at 5 range 0 .. 0;  --  reserved
      Stream_Id at 5 range 1 .. 31;
   end record;

   type Header_View (Flat : Boolean := False) is record
      case Flat is
         when False => H : Header;
         when True  => S : Stream_Element_Array (1 .. Header'Size / 8);
      end case;
   end record with Unchecked_Union;

   type Object is new Finalization.Controlled with record
      Header  : Header_View;
      Counter : Utils.Counter_Access; -- Standard ref counting
   end record;

   procedure Release (Self : in out Object) is null;
   --  The procedure release is to be overriden by the children and will be
   --  called when the frame object will have to free allocated memory.

   overriding procedure Initialize (Self : in out Object);
   overriding procedure Adjust (Self : in out Object);
   overriding procedure Finalize (Self : in out Object);

   --  Shared payload object in Data and Headers

   type Padding is new Byte_1 with Size => 8;

   function Is_Defined (Self : Object) return Boolean is
     (Self.Header.H /= Header'(others => <>));

   function Kind (Self : Object) return Kind_Type is
     (Self.Header.H.Kind);

   function Stream_Id (Self : Object) return HTTP2.Stream_Id is
     (Self.Header.H.Stream_Id);

   function Flags (Self : Object) return Flags_Type is
     (Self.Header.H.Flags);

   function Length (Self : Object) return Length_Type is
     (Self.Header.H.Length);

   function Has_Flag (Self : Object'Class; Flag : Flags_Type) return Boolean is
     ((Self.Header.H.Flags and Flag) = Flag);

end AWS.HTTP2.Frame;
