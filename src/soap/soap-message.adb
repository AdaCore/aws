------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2015, AdaCore                     --
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

with SOAP.Types;

package body SOAP.Message is

   ----------------
   -- Name_Space --
   ----------------

   function Name_Space (M : Object'Class) return SOAP.Name_Space.Object is
   begin
      return M.Name_Space;
   end Name_Space;

   ----------------
   -- Parameters --
   ----------------

   function Parameters (M : Object'Class) return SOAP.Parameters.List is
   begin
      return M.P;
   end Parameters;

   --------------------
   -- Set_Name_Space --
   --------------------

   procedure Set_Name_Space
     (M  : in out Object'Class;
      NS : SOAP.Name_Space.Object) is
   begin
      M.Name_Space := NS;
   end Set_Name_Space;

   --------------------
   -- Set_Parameters --
   --------------------

   procedure Set_Parameters
     (M     : in out Object'Class;
      P_Set : SOAP.Parameters.List) is
   begin
      M.P := P_Set;
   end Set_Parameters;

   ----------------------
   -- Set_Wrapper_Name --
   ----------------------

   procedure Set_Wrapper_Name
     (M    : in out Object'Class;
      Name : String) is
   begin
      M.Wrapper_Name := To_Unbounded_String (Name);
   end Set_Wrapper_Name;

   -----------
   -- Style --
   -----------

   function Style (M : Object'Class) return Binding_Style is
   begin
      return M.Style;
   end Style;

   ------------------
   -- Wrapper_Name --
   ------------------

   function Wrapper_Name (M : Object'Class) return String is
   begin
      return To_String (M.Wrapper_Name);
   end Wrapper_Name;

   ---------------
   -- XML_Image --
   ---------------

   function XML_Image (M : Object) return Unbounded_String is

      procedure Add_Namespaces (O : Types.Object'Class);
      --  Add name space reference into Message_Header

      New_Line       : constant String                 := ASCII.CR & ASCII.LF;
      NS             : constant SOAP.Name_Space.Object := Name_Space (M);
      NS_Name        : constant String                 :=
                         SOAP.Name_Space.Name (NS);
      Message_Header : Unbounded_String;
      Message_Body   : Unbounded_String;

      --------------------
      -- Add_Namespaces --
      --------------------

      procedure Add_Namespaces (O : Types.Object'Class) is
         use SOAP.Name_Space;
         use SOAP.Types;
         NS : constant SOAP.Name_Space.Object := Types.Name_Space (O);
      begin
         if NS /= No_Name_Space
           and then NS /= SOAP.Name_Space.AWS
           and then Index
             (Message_Header, ':' & SOAP.Name_Space.Name (NS) & '=') = 0
         then
            Append (Message_Header, " " & Image (NS));
         end if;

         --  If this is a composite object, check components

         if O in Types.Composite'Class then
            declare
               OS : constant Object_Set := V (Composite'Class (O));
            begin
               for K in OS'Range loop
                  Add_Namespaces (-OS (K));
               end loop;
            end;
         end if;
      end Add_Namespaces;

   begin
      --  Procedure

      if M.Style = RPC then
         Append (Message_Header, '<');

         if NS_Name /= "" then
            Append (Message_Header, NS_Name & ':');
         end if;

         Append (Message_Header, Wrapper_Name (M));

         Append (Message_Body, " xmlns");

         if NS_Name /= "" then
            Append (Message_Body, ":" & NS_Name);
         end if;

         Append
           (Message_Body,
            "=""" & SOAP.Name_Space.Value (NS) & """>" & New_Line);
      end if;

      --  Procedure's parameters

      declare
         P : constant SOAP.Parameters.List := Parameters (M);
      begin
         for K in 1 .. SOAP.Parameters.Argument_Count (P) loop
            Add_Namespaces (SOAP.Parameters.Argument (P, K));
            Types.XML_Image (SOAP.Parameters.Argument (P, K), Message_Body);
            Append (Message_Body, New_Line);
         end loop;
      end;

      --  Close payload objects

      if M.Style = RPC then
         Append (Message_Body, "</");

         if NS_Name /= "" then
            Append (Message_Body, NS_Name & ':');
         end if;

         Append (Message_Body, Wrapper_Name (M));
         Append (Message_Body, ">" & New_Line);
      end if;

      return Message_Header & Message_Body;
   end XML_Image;

end SOAP.Message;
