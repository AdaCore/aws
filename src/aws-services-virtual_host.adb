------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
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

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with AWS.MIME;
with AWS.Status;
with AWS.Parameters;

with Table_Of_Strings_And_Static_Values_G;

package body AWS.Services.Virtual_Host is

   use Ada;
   use Ada.Strings.Unbounded;

   type VH_Mode is (Host, Callback);

   type VH_Node (Mode : VH_Mode := Host) is record
      case Mode is
         when Host =>
            Hostname : Unbounded_String;
         when Callback =>
            CB       : Response.Callback;
      end case;
   end record;

   package Virtual_Host_Table is new Table_Of_Strings_And_Static_Values_G
     (Character, String, "<", "=", VH_Node);

   VH_Table : Virtual_Host_Table.Table_Type;

   function VH_Callback
     (Request : in AWS.Status.Data)
     return AWS.Response.Data;
   --  Virtual Hostname callback handling.

   function Default_Callback
     (Request : in AWS.Status.Data)
     return AWS.Response.Data;
   --  Default callback if no virtual host are activated.

   Def_Callback : Response.Callback := Default_Callback'Access;

   --------------
   -- Callback --
   --------------

   function Callback return Response.Callback is
   begin
      return VH_Callback'Access;
   end Callback;

   ----------------------
   -- Default_Callback --
   ----------------------

   function Default_Callback
     (Request : in AWS.Status.Data)
     return AWS.Response.Data is
   begin
      return AWS.Response.Build
        (MIME.Text_HTML,
         "<p>Virtual Hosting is activated but no virtual host match "
         & Status.Host (Request)
         & "<p>Please check your AWS Virtual Host configuration");
   end Default_Callback;

   --------------
   -- Register --
   --------------

   procedure Register
     (Virtual_Hostname : in String;
      Hostname         : in String)
   is
      Node : VH_Node := (Host, To_Unbounded_String (Hostname));
   begin
      Virtual_Host_Table.Insert_Or_Replace_Value
        (VH_Table, Virtual_Hostname, Node);
   end Register;

   --------------
   -- Register --
   --------------

   procedure Register
     (Virtual_Hostname : in String;
      Callback         : in Response.Callback)
   is
      Node : VH_Node := (Virtual_Host.Callback, Callback);
   begin
      Virtual_Host_Table.Insert_Or_Replace_Value
        (VH_Table, Virtual_Hostname, Node);
   end Register;

   -------------------------------
   -- Register_Default_Callback --
   -------------------------------

   procedure Register_Default_Callback (Callback : in Response.Callback) is
   begin
      Def_Callback := Callback;
   end Register_Default_Callback;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister (Virtual_Hostname : in String) is
   begin
      null;
   end Unregister;

   -----------------
   -- VH_Callback --
   -----------------

   function VH_Callback
     (Request : in AWS.Status.Data)
     return AWS.Response.Data
   is
      Hostname : constant String := Status.Host (Request);
      Location : Unbounded_String;
      K        : Natural;
      Node     : VH_Node;
   begin
      K := Strings.Fixed.Index (Hostname, ":");

      if K = 0 then
         K := Hostname'Last;
      else
         K := K - 1;
      end if;

      if Virtual_Host_Table.Is_Present
        (VH_Table, Hostname (Hostname'First .. K))
      then
         Virtual_Host_Table.Get_Value
           (VH_Table, Hostname (Hostname'First .. K), Node);

         case Node.Mode is
            when Host     =>

               declare
                  P : constant Parameters.List := Status.Parameters (Request);
               begin
                  Location := To_Unbounded_String ("http://");
                  Append (Location, To_String (Node.Hostname));
                  Append (Location, Status.URI (Request));
                  Append (Location, Parameters.URI_Format (P));
               end;

               return AWS.Response.URL (To_String (Location));

            when Callback =>
               return Node.CB (Request);
         end case;
      end if;

      return Def_Callback (Request);
   end VH_Callback;

end AWS.Services.Virtual_Host;
