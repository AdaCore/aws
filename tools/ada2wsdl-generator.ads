------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2003                            --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimkov - Pascal Obry                                --
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

package Ada2WSDL.Generator is

   -------------
   -- Routine --
   -------------

   procedure Start_Routine (Name, Comment : in String);
   --  Must be called when a new routine named Name has been parsed in the
   --  Ada spec file. Comment is displayed before the procedure name.

   procedure New_Formal (Var_Name, Var_Type : in String);
   --  Must be called for each formal parameter parsed. This new formal
   --  parameter will be added into the current routine definition created by
   --  Start_Routine.

   procedure Return_Type (Name : in String);
   --  Must be called when a returned type (for a function) has been
   --  parsed. It is fine to not call this routine, for example if current
   --  routine is a procedure there is no need to call it.

   ------------
   -- Record --
   ------------

   procedure Start_Record (Name : in String);
   --  Must be called when a new record named Name as been parsed

   procedure New_Component (Comp_Name, Comp_Type : in String);
   --  Must be called when a new component has been parsed. This component
   --  will be added into the current record definition created by
   --  Start_Record.

   -----------
   -- Array --
   -----------

   procedure Start_Array (Name, Component_Type : in String);
   --  Must be called when an array definition has been parsed

   -----------------
   -- Enumeration --
   -----------------

   procedure Start_Enumeration (Name : in String);
   --  Must be called when a new enumeration type named Name as been parsed

   procedure New_Literal (Name : in String);
   --  Must be called for each enumeration value parsed

   -----------
   -- Types --
   -----------

   procedure Register_Derived (Name, Parent_Name : in String);
   --  Register a derived type

   function Type_Exists (Name : in String) return Boolean;
   --  Returns True if Name exists in the type list

   ------------------
   -- Safe Pointer --
   ------------------

   procedure Register_Safe_Pointer (Name, Type_Name, Access_Name : in String);
   --  Add a new AWS/SOAP runtime safe pointer definition. Name is the name of
   --  the package instance, Type_Name and Access_Name the name of the type
   --  and access to the type respectively.

   ------------
   -- Output --
   ------------

   procedure Write (Filename : in String);
   --  Write out the WSDL document into Filename

end Ada2WSDL.Generator;
