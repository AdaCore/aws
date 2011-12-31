------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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

package body AWS.Resources.Streams.Memory is

   ------------
   -- Append --
   ------------

   procedure Append
     (Resource : in out Stream_Type;
      Buffer   : Stream_Element_Array;
      Trim     : Boolean := False) is
   begin
      Containers.Append (Resource.Data, Buffer, Trim);
   end Append;

   procedure Append
     (Resource : in out Stream_Type;
      Buffer   : Stream_Element_Access) is
   begin
      Containers.Append (Resource.Data, Buffer);
   end Append;

   procedure Append
     (Resource : in out Stream_Type;
      Buffer   : Buffer_Access) is
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

   overriding procedure Close (Resource : in out Stream_Type) is
   begin
      Containers.Close (Resource.Data);
   end Close;

   -----------------
   -- End_Of_File --
   -----------------

   overriding function End_Of_File
     (Resource : Stream_Type) return Boolean is
   begin
      return Containers.End_Of_File (Resource.Data);
   end End_Of_File;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Resource : in out Stream_Type;
      Buffer   : out Stream_Element_Array;
      Last     : out Stream_Element_Offset) is
   begin
      Containers.Read (Resource.Data, Buffer, Last);
   end Read;

   -----------
   -- Reset --
   -----------

   overriding procedure Reset (Resource : in out Stream_Type) is
   begin
      Containers.Reset (Resource.Data);
   end Reset;

   ---------------
   -- Set_Index --
   ---------------

   overriding procedure Set_Index
     (Resource : in out Stream_Type;
      To       : Stream_Element_Offset) is
   begin
      Containers.Set_Index (Resource.Data, To);
   end Set_Index;

   ----------
   -- Size --
   ----------

   overriding function Size
     (Resource : Stream_Type) return Stream_Element_Offset is
   begin
      return Containers.Size (Resource.Data);
   end Size;

end AWS.Resources.Streams.Memory;
