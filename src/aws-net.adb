------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2002                          --
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

with Ada.Unchecked_Deallocation;

with AWS.Net.Std;
with AWS.Net.SSL;

package body AWS.Net is

   use Ada;

   ----------
   -- Free --
   ----------

   procedure Free (Socket : in out Socket_Access) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Socket_Type'Class, Socket_Access);
   begin
      if Socket /= null then
         Release_Cache (Socket.all);
         Free (Socket.all);
         Free (Socket);
      end if;
   end Free;

   ---------------
   -- Host_Name --
   ---------------

   function Host_Name return String is
   begin
      return Net.Std.Host_Name;
   end Host_Name;

   -------------------
   -- Release_Cache --
   -------------------

   procedure Release_Cache (Socket : in out Socket_Type'Class) is
      procedure Free is
         new Ada.Unchecked_Deallocation (RW_Cache, RW_Cache_Access);
   begin
      Free (Socket.C);
   end Release_Cache;

   ---------------
   -- Set_Cache --
   ---------------

   procedure Set_Cache (Socket : in out Socket_Type'Class) is
   begin
      Socket.C := new RW_Cache;
   end Set_Cache;

   ------------
   -- Socket --
   ------------

   function Socket
     (Security : in Boolean)
      return Socket_Access
   is
      Sock : Socket_Access;
   begin
      if Security then
         Sock := SSL.Socket;
      else
         Sock := Std.Socket;
      end if;

      --  Create the cache structure
      Set_Cache (Sock.all);

      return Sock;
   end Socket;

end AWS.Net;
