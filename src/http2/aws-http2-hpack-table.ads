------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                      Copyright (C) 2021, AdaCore                         --
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

limited with AWS.HTTP2.Connection;

private with Ada.Containers.Indefinite_Vectors;
private with Ada.Containers.Indefinite_Ordered_Maps;

package AWS.HTTP2.HPACK.Table is

   type Object is tagged private;
   type Object_Access is access all Object;

   type Name_Value (N_Length : Positive; V_Length : Natural) is record
      Name  : String (1 .. N_Length);
      Value : String (1 .. V_Length);
   end record;

   procedure Insert
     (Self        : in out Object;
      Settings    : not null access HTTP2.Connection.Object;
      Name, Value : String);
   --  Insert Name & Value pair into the dynamic table

   function Get_Name (Self : Object; Index : Positive) return String;
   --  Get Name at the given index in the table

   function Get_Name_Value (Self : Object; Index : Positive) return Name_Value;
   --  Get Name & Value pair at the given index in the table

   function Get_Name_Value_Index
     (Self     : in out Object;
      Settings : not null access HTTP2.Connection.Object;
      Name     : String;
      Value    : String := "";
      Both     : out Boolean) return Natural;
   --  Get the index of the Name (and Value if specificed).
   --  Returns 0 if not found and adds Name to the internal table to search
   --  later. Both is set to true if the pair is found and False if only the
   --  Name has been found.

   function Size (Self : Object) return Natural;
   --  Returns size of the table in bytes

   procedure Dump (Self : Object);
   --  Dump table content for debug

   procedure Clear (Self : in out Object);
   --  Clear encoding/decoding dynamic table

private

   use Ada;

   package Index_NV is
     new Containers.Indefinite_Vectors (Positive, Name_Value);

   package NV_Index is
     new Containers.Indefinite_Ordered_Maps (String, Positive);

   type Static_Table is record
      T_IN : Index_NV.Vector;
      T_NI : NV_Index.Map;
   end record;

   type Dynamic_Table is record
      T_IN   : Index_NV.Vector;
      T_NI   : NV_Index.Map;
      Size   : Natural := 0; -- size in byte
      Length : Natural := 0; -- number of items
      Rank   : Natural := 0; -- rank (index entry) of item
   end record;

   type Object is tagged record
      Dynamic : Dynamic_Table;
   end record;

   function Size (Self : Object) return Natural is
     (Self.Dynamic.Size);

end AWS.HTTP2.HPACK.Table;
