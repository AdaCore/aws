------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2008, AdaCore                     --
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

with Ada.Finalization;
with Ada.Strings.Unbounded;

with AWS.Status;
with AWS.Response;
with SOAP.Types;
with SOAP.Message.Payload;

package SOAP.Utils is

   use Ada.Strings.Unbounded;

   function Tag (Name : in String; Start : in Boolean) return String;
   --  Returns XML tag named Name. If Start is True then an XML start element
   --  is returned otherwise an XML end element is returned.

   function Encode (Str : in String) return String;
   --  Encode XML entities and return the resulting string

   function NS (Name : in String) return String;
   --  Returns the namespace for Name, string prefix separated with a ':'

   function No_NS (Name : in String) return String;
   --  Returns Name without leading name space if present

   function With_NS (NS, Name : in String) return String;
   --  Returns NS:Name if NS is not empty otherwise just return Name

   function Is_Ada_Reserved_Word (Name : in String) return Boolean;
   --  Returns True if Name is an Ada reserved word

   function Time_Instant (TI, Name : in String) return Types.XSD_Time_Instant;
   --  Returns the timeInstant given an string encoded time

   ----------------------------------
   -- Basic_8bit string conversion --
   ----------------------------------

   function To_Utf8 (Str : in String) return String;
   pragma Inline (To_Utf8);
   function To_Utf8 (Str : in Unbounded_String) return Unbounded_String;
   --  Convert the Basic_8bit encoded Str string to Utf-8

   function From_Utf8 (Str : in String) return String;
   function From_Utf8 (Str : in Unbounded_String) return Unbounded_String;
   function From_Utf8 (Str : in String) return String_Access;
   pragma Inline (From_Utf8);
   --  Convert the Utf-8 encoded Str string to Basic_8bit

   -------------------------------
   --  SOAP Callback translator --
   -------------------------------

   generic
      with function SOAP_CB
        (SOAPAction : in String;
         Payload    : in Message.Payload.Object;
         Request    : in AWS.Status.Data) return AWS.Response.Data;
   function SOAP_Wrapper
     (Request : in AWS.Status.Data) return AWS.Response.Data;
   --  From a standard HTTP callback calls the SOAP callback passed as generic
   --  formal procedure. Raises Constraint_Error if Request is not a SOAP
   --  request.

   ------------------------------------
   -- SOAP Generator Runtime Support --
   ------------------------------------

   subtype SOAP_Base64 is String;

   generic
      type T is private;
      type T_Array is array (Positive range <>) of T;
      with function Get (O : in Types.Object'Class) return T;
   function To_T_Array (From : in Types.Object_Set) return T_Array;
   --  Convert a Types.Object_Set to an array of T

   generic
      type T is private;
      type Index is range <>;
      type T_Array is array (Index) of T;
      with function Get (O : in Types.Object'Class) return T;
   function To_T_Array_C (From : in Types.Object_Set) return T_Array;
   --  As above but for constrained arrays

   generic
      type T is private;
      type T_Array is array (Positive range <>) of T;
      type XSD_Type is new Types.Object with private;
      with function Get (V : in T; Name : in String := "item") return XSD_Type;
   function To_Object_Set (From : in T_Array) return Types.Object_Set;
   --  Convert an array of T to a Types.Object_Set

   generic
      type T is private;
      type Index is range <>;
      type T_Array is array (Index) of T;
      type XSD_Type is new Types.Object with private;
      with function Get (V : in T; Name : in String := "item") return XSD_Type;
   function To_Object_Set_C (From : in T_Array) return Types.Object_Set;
   --  As above but for constrained arrays

   function Get (Item : in Types.Object'Class) return Unbounded_String;
   --  Returns an Unbounded_String for Item. Item must be a SOAP string object

   function Get (Item : in Types.Object'Class) return Character;
   --  Returns a Character for Item. Item must be a SOAP string object

   function Get (Item : in Types.Object'Class) return String;
   --  Returns the string representation for enumeration Item

   function V (O : in Types.XSD_String) return Unbounded_String;
   --  Returns the Unbounded_String representation for the SOAP string
   --  parameter.

   function V (O : in Types.SOAP_Enumeration) return Character;
   --  Returns the character representation for the SOAP string
   --  parameter. This is supposed to be a string with a single character
   --  to map to Ada type.

   function Any
     (V    : in Types.XSD_Any_Type;
      Name : in String  := "item") return Types.XSD_Any_Type;
   --  Return V with the given name

   function US
     (V    : in Unbounded_String;
      Name : in String  := "item") return Types.XSD_String;
   --  Returns the SOAP string for the given Unbounded_String value and name

   function C
     (V    : in Character;
      Name : in String  := "item") return Types.SOAP_Enumeration;
   --  Returns the SOAP string for the given Character value and name

   --  Smart pointers support used for array access in SOAP record. The memory
   --  used by a safe pointer is released automatically when no more reference
   --  to the object exists.

   generic
      type T (<>) is private;
      type T_Access is access T;
   package Safe_Pointers is

      type Ref_Counter is private;

      type Safe_Pointer is new Ada.Finalization.Controlled with record
         Item : T_Access;
         Ref  : Ref_Counter;
      end record;

      overriding procedure Initialize (SP : in out Safe_Pointer);
      overriding procedure Adjust     (SP : in out Safe_Pointer);
      overriding procedure Finalize   (SP : in out Safe_Pointer);

      function To_Safe_Pointer (Item : in T) return Safe_Pointer;
      --  Returns a Safe_Pointer for object Item

   private

      type Ref_Counter is access Natural;

   end Safe_Pointers;

end SOAP.Utils;
