------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                          Copyright (C) 2000-2001                         --
--                      Dmitriy Anisimkov & Pascal Obry                     --
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

with Ada.Strings;

package body AWS.Status is

   use Ada.Strings;

   ------------------------
   -- Authorization_Name --
   ------------------------

   function Authorization_Name (D : in Data) return String is
   begin
      return To_String (D.Auth_Name);
   end Authorization_Name;

   ----------------------------
   -- Authorization_Password --
   ----------------------------

   function Authorization_Password (D : in Data) return String is
   begin
      return To_String (D.Auth_Password);
   end Authorization_Password;

   -----------------
   -- Binary_Data --
   -----------------

   function Binary_Data (D : in Data) return Stream_Element_Array is
   begin
      return D.Binary_Data.all;
   end Binary_Data;

   ----------------
   -- Connection --
   ----------------

   function Connection (D : in Data) return String is
   begin
      return To_String (D.Connection);
   end Connection;

   --------------------
   -- Content_Length --
   --------------------

   function Content_Length (D : in Data) return Natural is
   begin
      return D.Content_Length;
   end Content_Length;

   ------------------
   -- Content_Type --
   ------------------

   function Content_Type (D : in Data) return String is
   begin
      return To_String (D.Content_Type);
   end Content_Type;

   ---------------------
   -- File_Up_To_Date --
   ---------------------

   function File_Up_To_Date (D : in Data) return Boolean is
   begin
      return D.File_Up_To_Date;
   end File_Up_To_Date;

   -----------------
   -- Has_Session --
   -----------------

   function Has_Session (D : in Data) return Boolean is
   begin
      return D.Session_ID /= Null_Unbounded_String;
   end Has_Session;

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

   -----------------------
   -- If_Modified_Since --
   -----------------------

   function If_Modified_Since (D : in Data) return String is
   begin
      return To_String (D.If_Modified_Since);
   end If_Modified_Since;

   ------------
   -- Method --
   ------------

   function Method (D : in Data) return Request_Method is
   begin
      return D.Method;
   end Method;

   ------------------------
   -- Multipart_Boundary --
   ------------------------

   function Multipart_Boundary (D : in Data) return String is
   begin
      return To_String (D.Boundary);
   end Multipart_Boundary;

   ----------------
   -- Parameters --
   ----------------

   function Parameters (D : in Data) return AWS.Parameters.List is
   begin
      return D.Parameters;
   end Parameters;

   --------------
   -- Peername --
   --------------

   function Peername (D : in Data) return String is
   begin
      return To_String (D.Peername);
   end Peername;

   -------------
   -- Session --
   -------------

   function Session (D : in Data) return String is
   begin
      return To_String (D.Session_ID);
   end Session;

   function Session (D : in Data) return AWS.Session.ID is
   begin
      return AWS.Session.Value (Session (D));
   end Session;

   ---------
   -- URI --
   ---------

   function URI (D : in Data) return String is
   begin
      return To_String (D.URI);
   end URI;

end AWS.Status;
