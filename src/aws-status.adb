------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2000                            --
--                               Pascal Obry                                --
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

with Ada.Strings.Fixed;

package body AWS.Status is

   use Ada.Strings;

   ----------------
   -- Connection --
   ----------------

   function Connection (D : in Data) return String is
   begin
      return To_String (D.Connection);
   end Connection;

   ----------
   -- Host --
   ----------

   function Host (D : in Data) return String is
   begin
      return To_String (D.Host);
   end Host;

   ------------------
   -- HTTP_Version --
   ------------------

   function HTTP_Version (D : in Data) return String is
   begin
      return To_String (D.HTTP_Version);
   end HTTP_Version;

   ------------
   -- Method --
   ------------

   function Method (D : in Data) return Request_Method is
   begin
      return D.Method;
   end Method;

   ---------------
   -- Parameter --
   ---------------

   function Parameter (D : in Data; N : in Positive) return String is
      P : constant String := To_String (D.Parameters);
      I : Natural;
      S : Positive := 1;
      E : Natural;

   begin
      for K in 1 .. N loop
         I := Fixed.Index (P (S .. P'Last), "=");
         if I = 0 then
            return "";
         else
            S := I + 1;
         end if;
      end loop;

      E := Fixed.Index (P (S .. P'Last), "&");

      if E = 0 then
         --  last parameter
         return P (S .. P'Last);
      else
         return P (S .. E - 1);
      end if;
   end Parameter;

   function Parameter (D : in Data; Name : in String) return String is
      P : constant String := To_String (D.Parameters);
      I : Natural;
      S : Positive := 1;
      E : Natural;

   begin
      loop
         I := Fixed.Index (P (S .. P'Last), "=");
         if I = 0 then
            return "";
         else
            S := I + 1;
            if I - Name'Length > 0
              and then P (I - Name'Length .. I - 1) = Name
            then
               E := Fixed.Index (P (S .. P'Last), "&");

               if E = 0 then
                  --  last parameter
                  return P (S .. P'Last);
               else
                  return P (S .. E - 1);
               end if;
            end if;
         end if;
      end loop;
   end Parameter;

   --------------------
   -- Set_Connection --
   --------------------

   procedure Set_Connection (D : in out Data; Connection : in String) is
   begin
      D.Connection := To_Unbounded_String (Connection);
   end Set_Connection;

   --------------
   -- Set_Host --
   --------------

   procedure Set_Host (D : in out Data; Host : in String) is
   begin
      D.Host := To_Unbounded_String (Host);
   end Set_Host;

   -----------------
   -- Set_Request --
   -----------------

   procedure Set_Request (D            : in out Data;
                          Method       : in     Request_Method;
                          URI          : in     String;
                          HTTP_Version : in     String;
                          Parameters   : in     String := "") is
   begin
      D.Method       := Method;
      D.URI          := To_Unbounded_String (URI);
      D.HTTP_Version := To_Unbounded_String (HTTP_Version);
      D.Parameters   := To_Unbounded_String (Parameters);
   end Set_Request;

   ---------
   -- URI --
   ---------

   function URI (D : in Data) return String is
   begin
      return To_String (D.URI);
   end URI;

end AWS.Status;

