------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2002-2003                          --
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

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

package body AWS.Resources.Streams.Memory is

   ------------
   -- Append --
   ------------

   procedure Append
     (Resource : in out Stream_Type;
      Buffer   : in     Stream_Element_Array) is
   begin
      Containers.Append (Resource.Data, Buffer);
   end Append;

   procedure Append
     (Resource : in out Stream_Type;
      Buffer   : in     Stream_Element_Access) is
   begin
      Containers.Append (Resource.Data, Buffer);
   end Append;

   -----------
   -- Clear --
   -----------

   procedure Clear (Resource : in out Stream_Type) is
   begin
      Containers.Clear (Resource.Data);
   end Clear;

   -----------
   -- Close --
   -----------

   procedure Close (Resource : in out Stream_Type) is
   begin
      Containers.Close (Resource.Data);
   end Close;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File (Resource : in Stream_Type) return Boolean is
   begin
      return Containers.End_Of_File (Resource.Data);
   end End_Of_File;

   ----------
   -- Read --
   ----------

   procedure Read
     (Resource : in out Stream_Type;
      Buffer   :    out Stream_Element_Array;
      Last     :    out Stream_Element_Offset) is
   begin
      Containers.Read (Resource.Data, Buffer, Last);
   end Read;

   -----------
   -- Reset --
   -----------

   procedure Reset (Resource : in out Stream_Type) is
   begin
      Containers.Reset (Resource.Data);
   end Reset;

   ----------
   -- Size --
   ----------

   function Size (Resource : in Stream_Type) return Stream_Element_Offset is
   begin
      return Containers.Size (Resource.Data);
   end Size;

end AWS.Resources.Streams.Memory;
