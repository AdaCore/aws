------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2018, AdaCore                     --
--                                                                          --
--  This is free software;  you can redistribute it  and/or modify it       --
--  under terms of the  GNU General Public License as published  by the     --
--  Free Software  Foundation;  either version 3,  or (at your option) any  --
--  later version.  This software is distributed in the hope  that it will  --
--  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU     --
--  General Public License for  more details.                               --
--                                                                          --
--  You should have  received  a copy of the GNU General  Public  License   --
--  distributed  with  this  software;   see  file COPYING3.  If not, go    --
--  to http://www.gnu.org/licenses for a complete copy of the license.      --
------------------------------------------------------------------------------

with Ada.Strings.Unbounded;

package Ada2WSDL.Generator is

   use Ada.Strings.Unbounded;

   type Type_Data is record
      NS       : Unbounded_String;
      Name     : Unbounded_String;
      Min, Max : Unbounded_String;
      Len      : Unbounded_String;
   end record;

   -------------
   -- Routine --
   -------------

   procedure Start_Routine (NS, Name, Comment : String);
   --  Must be called when a new routine named Name has been parsed in the
   --  Ada spec file. Comment is displayed before the procedure name.

   procedure New_Formal (NS, Var_Name, Var_Type : String);
   --  Must be called for each formal parameter parsed. This new formal
   --  parameter will be added into the current routine definition created by
   --  Start_Routine.

   procedure Return_Type (NS, Name, Spec_Name : String);
   --  Must be called when a returned type (for a function) has been
   --  parsed. It is fine to not call this routine, for example if current
   --  routine is a procedure there is no need to call it.

   ------------
   -- Record --
   ------------

   procedure Start_Record (NS, Name : String);
   --  Must be called when a new record named Name as been parsed

   procedure New_Component (NS, Comp_Name, Comp_Type : String);
   --  Must be called when a new component has been parsed. This component
   --  will be added into the current record definition created by
   --  Start_Record.

   -----------
   -- Array --
   -----------

   procedure Start_Array
     (NS, Name                     : String;
      Component_NS, Component_Type : String;
      Length                   : Natural := 0);
   --  Must be called when an array definition has been parsed

   -----------------
   -- Enumeration --
   -----------------

   procedure Start_Enumeration (NS, Name : String);
   --  Must be called when a new enumeration type named Name as been parsed

   procedure New_Literal (Name : String);
   --  Must be called for each enumeration value parsed

   -----------
   -- Types --
   -----------

   procedure Register_Derived
     (NS, Name : String;
      Def      : Type_Data);
   --  Register a derived type

   procedure Register_Type
     (NS, Name : String;
      Def      : Type_Data);
   --  Register a user defined type

   function Type_Exists (NS, Name : String) return Boolean;
   --  Returns True if Name exists in the type list

   ------------------
   -- Safe Pointer --
   ------------------

   procedure Register_Safe_Pointer (Name, Type_Name, Access_Name : String);
   --  Add a new AWS/SOAP runtime safe pointer definition. Name is the name of
   --  the package instance, Type_Name and Access_Name the name of the type
   --  and access to the type respectively.

   ------------
   -- Output --
   ------------

   procedure Write (Filename : String);
   --  Write out the WSDL document into Filename

end Ada2WSDL.Generator;
