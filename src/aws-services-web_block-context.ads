------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2007-2009, AdaCore                     --
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

with AWS.Session;

package AWS.Services.Web_Block.Context is

   type Object is tagged private;
   --  A context object, can be used to record key/name values

   type Id is private;

   function Image (CID : Id) return String;
   --  Returns CID string representation

   function Value (CID : String) return Id;
   --  Returns Id given it's string representation

   function Create return Id;
   --  Create a new context and returns the corresponding Id

   procedure Copy (CID : Id; New_CID : Id);
   --  Copy a context

   function Copy (CID : Id) return Id;
   --  Returns a new context which is a copy of CID

   function Exist (CID : Id) return Boolean;
   --  Returns Trus if CID context exists into the database

   function Get (CID : Id) return Object;
   --  Returns the context object corresponding to CID

   procedure Set_Value (Context : in out Object; Name, Value : String);
   --  Add a new name/value pair

   function Get_Value (Context : Object; Name : String) return String;
   --  Get the value for the key Name

   generic
      type Data is private;
      Null_Data : Data;
   package Generic_Data is

      procedure Set_Value
        (Context : in out Object;
         Name    : String;
         Value   : Data);
      --  Set key/pair value for the SID

      function Get_Value (Context : Object; Name : String) return Data;
      pragma Inline (Get_Value);
      --  Returns the Value for Key in the session SID or Null_Data if
      --  key does not exist.

   end Generic_Data;

   function Exist (Context : Object; Name : String) return Boolean;
   --  Returns true if the key Name exist in this context

   procedure Remove (Context : Object; Name : String);
   --  Remove the context for key Name

private

   use AWS;

   type Id is new Session.Id;

   type Object is tagged record
      SID : Session.Id;
   end record;

end AWS.Services.Web_Block.Context;
