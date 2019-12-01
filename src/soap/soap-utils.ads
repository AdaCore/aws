------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2019, AdaCore                     --
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

pragma Ada_2012;

with Ada.Finalization;
with Ada.Strings.Unbounded;

with AWS.Response;
with AWS.Status;

with SOAP.Message.Payload;
with SOAP.Name_Space;
with SOAP.Types;
with SOAP.WSDL.Schema;

package SOAP.Utils is

   use Ada.Strings.Unbounded;

   function Tag (Name : String; Start : Boolean) return String;
   --  Returns XML tag named Name. If Start is True then an XML start element
   --  is returned otherwise an XML end element is returned.

   function Encode (Str : String) return String;
   --  Encode XML entities and return the resulting string
   procedure Encode (S : Unbounded_String; Result : in out Unbounded_String);
   --  Same as function, but append to Result

   function NS (Name : String) return String;
   --  Returns the namespace for Name, string prefix separated with a ':'

   function No_NS (Name : String) return String;
   --  Returns Name without leading name space if present

   function With_NS (NS, Name : String) return String;
   --  Returns NS:Name if NS is not empty otherwise just return Name

   function To_Name (Q_Name : String) return String;
   --  Returns a valid Ada name out of a fully qualified name

   function Is_Ada_Reserved_Word (Name : String) return Boolean;
   --  Returns True if Name is an Ada reserved word

   function Time_Instant
     (TI,
      Name      : String;
      Type_Name : String := Types.XML_Time_Instant)
      return Types.XSD_Time_Instant;
   --  Returns the timeInstant given an string encoded time

   function Duration
     (D, Name   : String;
      Type_Name : String := Types.XML_Duration)
      return Types.XSD_Duration
     with Pre => D'Length > 2
                 and then (D (D'First) = 'P'
                           or else
                          (D (D'First) = '-' and then D (D'First + 1) = 'P'));
   --  Returns the XSD_Duration given an string encoded time

   ----------------------------------
   -- Basic_8bit string conversion --
   ----------------------------------

   function To_Utf8 (Str : String) return String with Inline;
   function To_Utf8 (Str : Unbounded_String) return Unbounded_String;
   --  Convert the Basic_8bit encoded Str string to Utf-8

   function From_Utf8 (Str : String) return String with Inline;
   function From_Utf8 (Str : Unbounded_String) return Unbounded_String;
   function From_Utf8 (Str : String) return String_Access;
   --  Convert the Utf-8 encoded Str string to Basic_8bit

   -------------------------------
   --  SOAP Callback translator --
   -------------------------------

   generic
      with function SOAP_CB
        (SOAPAction : String;
         Payload    : Message.Payload.Object;
         Request    : AWS.Status.Data) return AWS.Response.Data;
   function SOAP_Wrapper
     (Request : AWS.Status.Data;
      Schema  : WSDL.Schema.Definition := WSDL.Schema.Empty)
      return AWS.Response.Data;
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
      with function Get (O : Types.Object'Class) return T;
   function To_T_Array (From : Types.Object_Set) return T_Array;
   --  Convert a Types.Object_Set to an array of T

   generic
      type T is private;
      type Index is range <>;
      type T_Array is array (Index) of T;
      with function Get (O : Types.Object'Class) return T;
   function To_T_Array_C (From : Types.Object_Set) return T_Array;
   --  As above but for constrained arrays

   generic
      type T is private;
      type T_Array is array (Positive range <>) of T;
      type XSD_Type is new Types.Object with private;
      E_Name    : String;
      Type_Name : String;
      with function
        Get (V         : T;
             Name      : String := "item";
             Type_Name : String := "";
             NS        : Name_Space.Object := Name_Space.No_Name_Space)
             return XSD_Type;
   function To_Object_Set
     (From : T_Array;
      NS   : Name_Space.Object) return Types.Object_Set;
   --  Convert an array of T to a Types.Object_Set

   generic
      type T is private;
      type Index is range <>;
      type T_Array is array (Index) of T;
      type XSD_Type is new Types.Object with private;
      E_Name    : String;
      Type_Name : String;
      with function
        Get (V         : T;
             Name      : String := "item";
             Type_Name : String := "";
             NS        : Name_Space.Object := Name_Space.No_Name_Space)
             return XSD_Type;
   function To_Object_Set_C
     (From : T_Array;
      NS   : Name_Space.Object) return Types.Object_Set;
   --  As above but for constrained arrays

   function Get (Item : Types.Object'Class) return Unbounded_String;
   --  Returns an Unbounded_String for Item. Item must be a SOAP string object

   function Get (Item : Types.Object'Class) return Character;
   --  Returns a Character for Item. Item must be a SOAP string object

   function Get (Item : Types.Object'Class) return String;
   --  Returns the string representation for enumeration Item

   function V (O : Types.XSD_String) return Unbounded_String;
   --  Returns the Unbounded_String representation for the SOAP string
   --  parameter.

   function V (O : Types.XSD_String) return Character;
   --  Returns the character representation for the SOAP string
   --  parameter. This is supposed to be a string with a single character
   --  to map to Ada type.

   function Any
     (V         : Types.XSD_Any_Type;
      Name      : String := "item";
      Type_Name : String := Types.XML_String;
      NS        : Name_Space.Object := Name_Space.No_Name_Space)
      return Types.XSD_Any_Type;
   --  Return V with the given name

   function US
     (V         : Unbounded_String;
      Name      : String := "item";
      Type_Name : String := Types.XML_String;
      NS        : Name_Space.Object := Name_Space.No_Name_Space)
      return Types.XSD_String;
   --  Returns the SOAP string for the given Unbounded_String value and name

   function C
     (V         : Character;
      Name      : String := "item";
      Type_Name : String := "Character";
      NS        : Name_Space.Object := Name_Space.No_Name_Space)
      return Types.XSD_String;
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

      function To_Safe_Pointer (Item : T) return Safe_Pointer;
      --  Returns a Safe_Pointer for object Item

   private

      type Ref_Counter is access Natural;

   end Safe_Pointers;

end SOAP.Utils;
