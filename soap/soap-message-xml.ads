------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimov - Pascal Obry                                 --
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

--  $Id$

with Sax.Readers;
with Sax.Attributes;
with Unicode.CES;

with SOAP.Message.Payload;
with SOAP.Message.Response.Error;

package SOAP.Message.XML is

   function Load_Payload
     (XML : in String)
     return Message.Payload.Object;
   --  Build a Payload object by parsing the XML payload string.

   function Load_Response
     (XML : in String)
     return Message.Response.Object'Class;
   --  Build a Response object (either a standard response or an error
   --  response) by parsing the XML response string.

   function Image (O : in Object'Class) return String;
   --  Returns XML representation of object O.

   function Image (O : in Object'Class) return Unbounded_String;
   --  Idem as above but returns an Unbounded_String instead of a String.

private

   ---------------------------
   -- SAX reader definition --
   ---------------------------

   type State is
     (Start,   -- Nothing parsed yet
      S_Env,   -- Start of SOAP envelope
      S_Body,  -- Start of SOAP body
      S_Wrap,  -- Start of Wrapper (procedure or SOAP:Fault)
      P_Int,   -- Integer declaration found, next object will be the value
      P_Float, -- Float declaration found, next object will be the value
      P_Str,   -- String declaration found, next object will be the value
      E_Wrap,  -- End of Wrapper (procedure or SOAP:Fault)
      E_Body,  -- End of SOAP body
      E_Env);  -- End of SOAP envelope

   subtype State_Param is State
     range State'Succ (S_Wrap) .. State'Pred (E_Wrap);

   -----------------
   -- SOAP Reader --
   -----------------

   type SOAP_Reader is new Sax.Readers.Reader with record
      S : State := Start;

      Parameters   : SOAP.Parameters.Set;
      Last_Name    : Unbounded_String;
      Last_Str     : Unbounded_String;
      Wrapper_Name : Unbounded_String;
   end record;

   function Is_Error (Handler : in SOAP_Reader) return Boolean;

   procedure Characters
     (Handler : in out SOAP_Reader;
      Ch      : in     Unicode.CES.Byte_Sequence);

   procedure Set_Wrapper_Name
     (Handler : in out SOAP_Reader;
      Name    : in     String);

   procedure Start_Element
     (Handler       : in out SOAP_Reader;
      Namespace_URI : in     Unicode.CES.Byte_Sequence       := "";
      Local_Name    : in     Unicode.CES.Byte_Sequence       := "";
      Qname         : in     Unicode.CES.Byte_Sequence       := "";
      Atts          : in     Sax.Attributes.Attributes'Class);

   procedure End_Element
     (Handler       : in out SOAP_Reader;
      Namespace_URI : in     Unicode.CES.Byte_Sequence := "";
      Local_Name    : in     Unicode.CES.Byte_Sequence := "";
      Qname         : in     Unicode.CES.Byte_Sequence := "");

   --------------------
   -- Payload Reader --
   --------------------

   type Payload_Reader is new SOAP_Reader with record
      Payload : Message.Payload.Object;
   end record;

   function Is_Error (Handler : in Payload_Reader) return Boolean;

   procedure Set_Wrapper_Name
     (Handler : in out Payload_Reader;
      Name    : in     String);

   ---------------------
   -- Response Reader --
   ---------------------

   type Response_Reader is new SOAP_Reader with record
      Is_Error : Boolean := False;
      Response : Message.Response.Error.Object;
   end record;

   function Is_Error (Handler : in Response_Reader) return Boolean;

   procedure Set_Wrapper_Name
     (Handler : in out Response_Reader;
      Name    : in     String);

end SOAP.Message.XML;
