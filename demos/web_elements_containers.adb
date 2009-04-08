------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2009, AdaCore                     --
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

with Ada.Containers.Indefinite_Vectors;

with AWS.Templates;

package body Web_Elements_Containers is

   use AWS.Templates;

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   Groups : String_Vectors.Vector;
   Users  : String_Vectors.Vector;

   ---------------
   -- Add_Group --
   ---------------

   procedure Add_Group (Name : String) is
   begin
      String_Vectors.Append (Groups, Name);
   end Add_Group;

   --------------
   -- Add_User --
   --------------

   procedure Add_User (Name : String) is
   begin
      String_Vectors.Append (Users, Name);
   end Add_User;

   ----------------
   -- Get_Groups --
   ----------------

   function Get_Groups return AWS.Templates.Vector_Tag is
      Result : Vector_Tag;
   begin
      for K in 1 .. String_Vectors.Length (Groups) loop
         Result := Result & String_Vectors.Element (Groups, Positive (K));
      end loop;

      return Result;
   end Get_Groups;

   ---------------
   -- Get_Users --
   ---------------

   function Get_Users return AWS.Templates.Vector_Tag is
      Result : Vector_Tag;
   begin
      for K in 1 .. String_Vectors.Length (Users) loop
         Result := Result & String_Vectors.Element (Users, Positive (K));
      end loop;

      return Result;
   end Get_Users;

end Web_Elements_Containers;
