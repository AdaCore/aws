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

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

package body AWS.Services.Web_Block.Context is

   use Ada;
   use Ada.Strings.Unbounded;

   ----------
   -- Copy --
   ----------

   function Copy (CID : Id) return Id is
      New_CID : constant Id := Create;
   begin
      Copy (CID, New_CID);
      return New_CID;
   end Copy;

   procedure Copy (CID : Id; New_CID : Id) is

      --   Note that we first copy the context into a vector and set the new
      --   context from it. This is because it is not possible to call
      --   Set_Value from inside the For_Every_Session_Data callback.

      type Context_Data is record
         Key, Value : Unbounded_String;
      end record;

      package CD_Vector is new Containers.Vectors (Positive, Context_Data);
      use CD_Vector;

      O       : Object := Get (New_CID);
      Data    : Vector;

      procedure Insert
        (N          : Positive;
         Key, Value : String;
         Quit       : in out Boolean);
      --  Insert key/value into O

      ------------
      -- Insert --
      ------------

      procedure Insert
        (N          : Positive;
         Key, Value : String;
         Quit       : in out Boolean)
      is
         pragma Unreferenced (N, Quit);
      begin
         Append
           (Container => Data,
            New_Item  => Context_Data'(Key   => To_Unbounded_String (Key),
                                       Value => To_Unbounded_String (Value)));
      end Insert;

      ------------------
      -- Copy_Context --
      ------------------

      procedure Copy_Context is new Session.For_Every_Session_Data (Insert);

   begin
      Copy_Context (Session.Id (CID));

      --  Now create the new context

      for K in Positive range 1 .. Natural (Length (Data)) loop
         declare
            D : constant Context_Data := Element (Data, K);
         begin
            Set_Value (O, To_String (D.Key), To_String (D.Value));
         end;
      end loop;
   end Copy;

   ------------
   -- Create --
   ------------

   overriding function Create return Id is
   begin
      return Id (Session.Create);
   end Create;

   -----------
   -- Exist --
   -----------

   overriding function Exist (CID : Id) return Boolean is
   begin
      return Session.Exist (Session.Id (CID));
   end Exist;

   function Exist (Context : Object; Name : String) return Boolean is
   begin
      return Session.Exist (Context.SID, Name);
   end Exist;

   ---------
   -- Get --
   ---------

   function Get (CID : Id) return Object is
   begin
      return Object'(SID => Session.Id (CID));
   end Get;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Context : Object; Name : String) return String is
   begin
      return Session.Get (Context.SID, Name);
   end Get_Value;

   -----------
   -- Image --
   -----------

   overriding function Image (CID : Id) return String is
   begin
      return Session.Image (Session.Id (CID));
   end Image;

   ------------
   -- Remove --
   ------------

   procedure Remove (Context : Object; Name : String) is
   begin
      Session.Remove (Context.SID, Name);
   end Remove;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (Context : in out Object; Name, Value : String) is
   begin
      Session.Set (Context.SID, Name, Value);
   end Set_Value;

   -----------
   -- Value --
   -----------

   overriding function Value (CID : String) return Id is
   begin
      return Id (Session.Value (CID));
   exception
      when Constraint_Error =>
         return Id (Session.No_Session);
   end Value;

   ------------------
   -- Generic_Data --
   ------------------

   package body Generic_Data is

      package Context_Generic_Data is
        new Session.Generic_Data (Data, Null_Data);
      use Context_Generic_Data;

      ---------------
      -- Get_Value --
      ---------------

      function Get_Value (Context : Object; Name : String) return Data is
      begin
         return Get (Context.SID, Name);
      end Get_Value;

      ---------------
      -- Set_Value --
      ---------------

      procedure Set_Value
        (Context : in out Object;
         Name    : String;
         Value   : Data) is
      begin
         Set (Context.SID, Name, Value);
      end Set_Value;

   end Generic_Data;

end AWS.Services.Web_Block.Context;
